/*
 * Builtin "git branch"
 *
 * Copyright (c) 2006 Kristian Høgsberg <krh@redhat.com>
 * Based on git-branch.sh by Junio C Hamano.
 */

#include "cache.h"
#include "color.h"
#include "refs.h"
#include "commit.h"
#include "builtin.h"
#include "remote.h"
#include "parse-options.h"
#include "branch.h"
#include "diff.h"
#include "revision.h"
#include "string-list.h"
#include "column.h"
#include "utf8.h"
#include "wt-status.h"

static const char * const builtin_branch_usage[] = {
	N_("git branch [options] [-r | -a] [--merged | --no-merged]"),
	N_("git branch [options] [-l] [-f] <branchname> [<start-point>]"),
	N_("git branch [options] [-r] (-d | -D) <branchname>..."),
	N_("git branch [options] (-m | -M) [<oldbranch>] <newbranch>"),
	NULL
};

#define REF_LOCAL_BRANCH    0x01
#define REF_REMOTE_BRANCH   0x02

static const char *head;
static unsigned char head_sha1[20];

static int branch_use_color = -1;
static char branch_colors[][COLOR_MAXLEN] = {
	GIT_COLOR_RESET,
	GIT_COLOR_NORMAL,	/* PLAIN */
	GIT_COLOR_RED,		/* REMOTE */
	GIT_COLOR_NORMAL,	/* LOCAL */
	GIT_COLOR_GREEN,	/* CURRENT */
	GIT_COLOR_BLUE,		/* UPSTREAM */
};
enum color_branch {
	BRANCH_COLOR_RESET = 0,
	BRANCH_COLOR_PLAIN = 1,
	BRANCH_COLOR_REMOTE = 2,
	BRANCH_COLOR_LOCAL = 3,
	BRANCH_COLOR_CURRENT = 4,
	BRANCH_COLOR_UPSTREAM = 5
};

static enum merge_filter {
	NO_FILTER = 0,
	SHOW_NOT_MERGED,
	SHOW_MERGED
} merge_filter;
static unsigned char merge_filter_ref[20];

static struct string_list output = STRING_LIST_INIT_DUP;
static unsigned int colopts;

static int parse_branch_color_slot(const char *var, int ofs)
{
	if (!strcasecmp(var+ofs, "plain"))
		return BRANCH_COLOR_PLAIN;
	if (!strcasecmp(var+ofs, "reset"))
		return BRANCH_COLOR_RESET;
	if (!strcasecmp(var+ofs, "remote"))
		return BRANCH_COLOR_REMOTE;
	if (!strcasecmp(var+ofs, "local"))
		return BRANCH_COLOR_LOCAL;
	if (!strcasecmp(var+ofs, "current"))
		return BRANCH_COLOR_CURRENT;
	if (!strcasecmp(var+ofs, "upstream"))
		return BRANCH_COLOR_UPSTREAM;
	return -1;
}

static int git_branch_config(const char *var, const char *value, void *cb)
{
	if (starts_with(var, "column."))
		return git_column_config(var, value, "branch", &colopts);
	if (!strcmp(var, "color.branch")) {
		branch_use_color = git_config_colorbool(var, value);
		return 0;
	}
	if (starts_with(var, "color.branch.")) {
		int slot = parse_branch_color_slot(var, 13);
		if (slot < 0)
			return 0;
		if (!value)
			return config_error_nonbool(var);
		color_parse(value, var, branch_colors[slot]);
		return 0;
	}
	return git_color_default_config(var, value, cb);
}

static const char *branch_get_color(enum color_branch ix)
{
	if (want_color(branch_use_color))
		return branch_colors[ix];
	return "";
}

static int branch_merged(int kind, const char *name,
			 struct commit *rev, struct commit *head_rev)
{
	/*
	 * This checks whether the merge bases of branch and HEAD (or
	 * the other branch this branch builds upon) contains the
	 * branch, which means that the branch has already been merged
	 * safely to HEAD (or the other branch).
	 */
	struct commit *reference_rev = NULL;
	const char *reference_name = NULL;
	void *reference_name_to_free = NULL;
	int merged;

	if (kind == REF_LOCAL_BRANCH) {
		struct branch *branch = branch_get(name);
		unsigned char sha1[20];

		if (branch &&
		    branch->merge &&
		    branch->merge[0] &&
		    branch->merge[0]->dst &&
		    (reference_name = reference_name_to_free =
		     resolve_refdup(branch->merge[0]->dst, sha1, 1, NULL)) != NULL)
			reference_rev = lookup_commit_reference(sha1);
	}
	if (!reference_rev)
		reference_rev = head_rev;

	merged = in_merge_bases(rev, reference_rev);

	/*
	 * After the safety valve is fully redefined to "check with
	 * upstream, if any, otherwise with HEAD", we should just
	 * return the result of the in_merge_bases() above without
	 * any of the following code, but during the transition period,
	 * a gentle reminder is in order.
	 */
	if ((head_rev != reference_rev) &&
	    in_merge_bases(rev, head_rev) != merged) {
		if (merged)
			warning(_("deleting branch '%s' that has been merged to\n"
				"         '%s', but not yet merged to HEAD."),
				name, reference_name);
		else
			warning(_("not deleting branch '%s' that is not yet merged to\n"
				"         '%s', even though it is merged to HEAD."),
				name, reference_name);
	}
	free(reference_name_to_free);
	return merged;
}

static int check_branch_commit(const char *branchname, const char *refname,
			       unsigned char *sha1, struct commit *head_rev,
			       int kinds, int force)
{
	struct commit *rev = lookup_commit_reference(sha1);
	if (!rev) {
		error(_("Couldn't look up commit object for '%s'"), refname);
		return -1;
	}
	if (!force && !branch_merged(kinds, branchname, rev, head_rev)) {
		error(_("The branch '%s' is not fully merged.\n"
		      "If you are sure you want to delete it, "
		      "run 'git branch -D %s'."), branchname, branchname);
		return -1;
	}
	return 0;
}

static void delete_branch_config(const char *branchname)
{
	struct strbuf buf = STRBUF_INIT;
	strbuf_addf(&buf, "branch.%s", branchname);
	if (git_config_rename_section(buf.buf, NULL) < 0)
		warning(_("Update of config-file failed"));
	strbuf_release(&buf);
}

static int delete_branches(int argc, const char **argv, int force, int kinds,
			   int quiet)
{
	struct commit *head_rev = NULL;
	unsigned char sha1[20];
	char *name = NULL;
	const char *fmt;
	int i;
	int ret = 0;
	int remote_branch = 0;
	struct strbuf bname = STRBUF_INIT;

	switch (kinds) {
	case REF_REMOTE_BRANCH:
		fmt = "refs/remotes/%s";
		/* For subsequent UI messages */
		remote_branch = 1;

		force = 1;
		break;
	case REF_LOCAL_BRANCH:
		fmt = "refs/heads/%s";
		break;
	default:
		die(_("cannot use -a with -d"));
	}

	if (!force) {
		head_rev = lookup_commit_reference(head_sha1);
		if (!head_rev)
			die(_("Couldn't look up commit object for HEAD"));
	}
	for (i = 0; i < argc; i++, strbuf_release(&bname)) {
		const char *target;
		int flags = 0;

		strbuf_branchname(&bname, argv[i]);
		if (kinds == REF_LOCAL_BRANCH && !strcmp(head, bname.buf)) {
			error(_("Cannot delete the branch '%s' "
			      "which you are currently on."), bname.buf);
			ret = 1;
			continue;
		}

		free(name);

		name = mkpathdup(fmt, bname.buf);
		target = resolve_ref_unsafe(name, sha1, 0, &flags);
		if (!target ||
		    (!(flags & REF_ISSYMREF) && is_null_sha1(sha1))) {
			error(remote_branch
			      ? _("remote branch '%s' not found.")
			      : _("branch '%s' not found."), bname.buf);
			ret = 1;
			continue;
		}

		if (!(flags & REF_ISSYMREF) &&
		    check_branch_commit(bname.buf, name, sha1, head_rev, kinds,
					force)) {
			ret = 1;
			continue;
		}

		if (delete_ref(name, sha1, REF_NODEREF)) {
			error(remote_branch
			      ? _("Error deleting remote branch '%s'")
			      : _("Error deleting branch '%s'"),
			      bname.buf);
			ret = 1;
			continue;
		}
		if (!quiet) {
			printf(remote_branch
			       ? _("Deleted remote branch %s (was %s).\n")
			       : _("Deleted branch %s (was %s).\n"),
			       bname.buf,
			       (flags & REF_ISSYMREF)
			       ? target
			       : find_unique_abbrev(sha1, DEFAULT_ABBREV));
		}
		delete_branch_config(bname.buf);
	}

	free(name);

	return(ret);
}

struct ref_item {
	char *name;
	char *dest;
	unsigned int kind, width;
	struct commit *commit;
};

struct ref_list {
	struct rev_info revs;
	int index, alloc, maxwidth, verbose, abbrev;
	struct ref_item *list;
	struct commit_list *with_commit;
	int kinds;
};

static char *resolve_symref(const char *src, const char *prefix)
{
	unsigned char sha1[20];
	int flag;
	const char *dst, *cp;

	dst = resolve_ref_unsafe(src, sha1, 0, &flag);
	if (!(dst && (flag & REF_ISSYMREF)))
		return NULL;
	if (prefix && (cp = skip_prefix(dst, prefix)))
		dst = cp;
	return xstrdup(dst);
}

struct append_ref_cb {
	struct ref_list *ref_list;
	const char **pattern;
	int ret;
};

static int match_patterns(const char **pattern, const char *refname)
{
	if (!*pattern)
		return 1; /* no pattern always matches */
	while (*pattern) {
		if (!fnmatch(*pattern, refname, 0))
			return 1;
		pattern++;
	}
	return 0;
}

static int append_ref(const char *refname, const unsigned char *sha1, int flags, void *cb_data)
{
	struct append_ref_cb *cb = (struct append_ref_cb *)(cb_data);
	struct ref_list *ref_list = cb->ref_list;
	struct ref_item *newitem;
	struct commit *commit;
	int kind, i;
	const char *prefix, *orig_refname = refname;

	static struct {
		int kind;
		const char *prefix;
		int pfxlen;
	} ref_kind[] = {
		{ REF_LOCAL_BRANCH, "refs/heads/", 11 },
		{ REF_REMOTE_BRANCH, "refs/remotes/", 13 },
	};

	/* Detect kind */
	for (i = 0; i < ARRAY_SIZE(ref_kind); i++) {
		prefix = ref_kind[i].prefix;
		if (strncmp(refname, prefix, ref_kind[i].pfxlen))
			continue;
		kind = ref_kind[i].kind;
		refname += ref_kind[i].pfxlen;
		break;
	}
	if (ARRAY_SIZE(ref_kind) <= i)
		return 0;

	/* Don't add types the caller doesn't want */
	if ((kind & ref_list->kinds) == 0)
		return 0;

	if (!match_patterns(cb->pattern, refname))
		return 0;

	commit = NULL;
	if (ref_list->verbose || ref_list->with_commit || merge_filter != NO_FILTER) {
		commit = lookup_commit_reference_gently(sha1, 1);
		if (!commit) {
			cb->ret = error(_("branch '%s' does not point at a commit"), refname);
			return 0;
		}

		/* Filter with with_commit if specified */
		if (!is_descendant_of(commit, ref_list->with_commit))
			return 0;

		if (merge_filter != NO_FILTER)
			add_pending_object(&ref_list->revs,
					   (struct object *)commit, refname);
	}

	ALLOC_GROW(ref_list->list, ref_list->index + 1, ref_list->alloc);

	/* Record the new item */
	newitem = &(ref_list->list[ref_list->index++]);
	newitem->name = xstrdup(refname);
	newitem->kind = kind;
	newitem->commit = commit;
	newitem->width = utf8_strwidth(refname);
	newitem->dest = resolve_symref(orig_refname, prefix);
	/* adjust for "remotes/" */
	if (newitem->kind == REF_REMOTE_BRANCH &&
	    ref_list->kinds != REF_REMOTE_BRANCH)
		newitem->width += 8;
	if (newitem->width > ref_list->maxwidth)
		ref_list->maxwidth = newitem->width;

	return 0;
}

static void free_ref_list(struct ref_list *ref_list)
{
	int i;

	for (i = 0; i < ref_list->index; i++) {
		free(ref_list->list[i].name);
		free(ref_list->list[i].dest);
	}
	free(ref_list->list);
}

static int ref_cmp(const void *r1, const void *r2)
{
	struct ref_item *c1 = (struct ref_item *)(r1);
	struct ref_item *c2 = (struct ref_item *)(r2);

	if (c1->kind != c2->kind)
		return c1->kind - c2->kind;
	return strcmp(c1->name, c2->name);
}

static void fill_tracking_info(struct strbuf *stat, const char *branch_name,
		int show_upstream_ref)
{
	int ours, theirs;
	char *ref = NULL;
	struct branch *branch = branch_get(branch_name);
	struct strbuf fancy = STRBUF_INIT;
	int upstream_is_gone = 0;
	int added_decoration = 1;

	switch (stat_tracking_info(branch, &ours, &theirs)) {
	case 0:
		/* no base */
		return;
	case -1:
		/* with "gone" base */
		upstream_is_gone = 1;
		break;
	default:
		/* with base */
		break;
	}

	if (show_upstream_ref) {
		ref = shorten_unambiguous_ref(branch->merge[0]->dst, 0);
		if (want_color(branch_use_color))
			strbuf_addf(&fancy, "%s%s%s",
					branch_get_color(BRANCH_COLOR_UPSTREAM),
					ref, branch_get_color(BRANCH_COLOR_RESET));
		else
			strbuf_addstr(&fancy, ref);
	}

	if (upstream_is_gone) {
		if (show_upstream_ref)
			strbuf_addf(stat, _("[%s: gone]"), fancy.buf);
		else
			added_decoration = 0;
	} else if (!ours && !theirs) {
		if (show_upstream_ref)
			strbuf_addf(stat, _("[%s]"), fancy.buf);
		else
			added_decoration = 0;
	} else if (!ours) {
		if (show_upstream_ref)
			strbuf_addf(stat, _("[%s: behind %d]"), fancy.buf, theirs);
		else
			strbuf_addf(stat, _("[behind %d]"), theirs);

	} else if (!theirs) {
		if (show_upstream_ref)
			strbuf_addf(stat, _("[%s: ahead %d]"), fancy.buf, ours);
		else
			strbuf_addf(stat, _("[ahead %d]"), ours);
	} else {
		if (show_upstream_ref)
			strbuf_addf(stat, _("[%s: ahead %d, behind %d]"),
				    fancy.buf, ours, theirs);
		else
			strbuf_addf(stat, _("[ahead %d, behind %d]"),
				    ours, theirs);
	}
	strbuf_release(&fancy);
	if (added_decoration)
		strbuf_addch(stat, ' ');
	free(ref);
}

static int matches_merge_filter(struct commit *commit)
{
	int is_merged;

	if (merge_filter == NO_FILTER)
		return 1;

	is_merged = !!(commit->object.flags & UNINTERESTING);
	return (is_merged == (merge_filter == SHOW_MERGED));
}

static void add_verbose_info(struct strbuf *out, struct ref_item *item,
			     int verbose, int abbrev)
{
	struct strbuf subject = STRBUF_INIT, stat = STRBUF_INIT;
	const char *sub = _(" **** invalid ref ****");
	struct commit *commit = item->commit;

	if (!parse_commit(commit)) {
		pp_commit_easy(CMIT_FMT_ONELINE, commit, &subject);
		sub = subject.buf;
	}

	if (item->kind == REF_LOCAL_BRANCH)
		fill_tracking_info(&stat, item->name, verbose > 1);

	strbuf_addf(out, " %s %s%s",
		find_unique_abbrev(item->commit->object.sha1, abbrev),
		stat.buf, sub);
	strbuf_release(&stat);
	strbuf_release(&subject);
}

static void print_ref_item(struct ref_item *item, int maxwidth, int verbose,
			   int abbrev, int current, char *prefix)
{
	char c;
	int color;
	struct commit *commit = item->commit;
	struct strbuf out = STRBUF_INIT, name = STRBUF_INIT;

	if (!matches_merge_filter(commit))
		return;

	switch (item->kind) {
	case REF_LOCAL_BRANCH:
		color = BRANCH_COLOR_LOCAL;
		break;
	case REF_REMOTE_BRANCH:
		color = BRANCH_COLOR_REMOTE;
		break;
	default:
		color = BRANCH_COLOR_PLAIN;
		break;
	}

	c = ' ';
	if (current) {
		c = '*';
		color = BRANCH_COLOR_CURRENT;
	}

	strbuf_addf(&name, "%s%s", prefix, item->name);
	if (verbose) {
		int utf8_compensation = strlen(name.buf) - utf8_strwidth(name.buf);
		strbuf_addf(&out, "%c %s%-*s%s", c, branch_get_color(color),
			    maxwidth + utf8_compensation, name.buf,
			    branch_get_color(BRANCH_COLOR_RESET));
	} else
		strbuf_addf(&out, "%c %s%s%s", c, branch_get_color(color),
			    name.buf, branch_get_color(BRANCH_COLOR_RESET));

	if (item->dest)
		strbuf_addf(&out, " -> %s", item->dest);
	else if (verbose)
		/* " f7c0c00 [ahead 58, behind 197] vcs-svn: drop obj_pool.h" */
		add_verbose_info(&out, item, verbose, abbrev);
	if (column_active(colopts)) {
		assert(!verbose && "--column and --verbose are incompatible");
		string_list_append(&output, out.buf);
	} else {
		printf("%s\n", out.buf);
	}
	strbuf_release(&name);
	strbuf_release(&out);
}

static int calc_maxwidth(struct ref_list *refs)
{
	int i, w = 0;
	for (i = 0; i < refs->index; i++) {
		if (!matches_merge_filter(refs->list[i].commit))
			continue;
		if (refs->list[i].width > w)
			w = refs->list[i].width;
	}
	return w;
}

static char *get_head_description(void)
{
	struct strbuf desc = STRBUF_INIT;
	struct wt_status_state state;
	memset(&state, 0, sizeof(state));
	wt_status_get_state(&state, 1);
	if (state.rebase_in_progress ||
	    state.rebase_interactive_in_progress)
		strbuf_addf(&desc, _("(no branch, rebasing %s)"),
			    state.branch);
	else if (state.bisect_in_progress)
		strbuf_addf(&desc, _("(no branch, bisect started on %s)"),
			    state.branch);
	else if (state.detached_from)
		strbuf_addf(&desc, _("(detached from %s)"),
			    state.detached_from);
	else
		strbuf_addstr(&desc, _("(no branch)"));
	free(state.branch);
	free(state.onto);
	free(state.detached_from);
	return strbuf_detach(&desc, NULL);
}

static void show_detached(struct ref_list *ref_list)
{
	struct commit *head_commit = lookup_commit_reference_gently(head_sha1, 1);

	if (head_commit && is_descendant_of(head_commit, ref_list->with_commit)) {
		struct ref_item item;
		item.name = get_head_description();
		item.width = utf8_strwidth(item.name);
		item.kind = REF_LOCAL_BRANCH;
		item.dest = NULL;
		item.commit = head_commit;
		if (item.width > ref_list->maxwidth)
			ref_list->maxwidth = item.width;
		print_ref_item(&item, ref_list->maxwidth, ref_list->verbose, ref_list->abbrev, 1, "");
		free(item.name);
	}
}

static int print_ref_list(int kinds, int detached, int verbose, int abbrev, struct commit_list *with_commit, const char **pattern)
{
	int i;
	struct append_ref_cb cb;
	struct ref_list ref_list;

	memset(&ref_list, 0, sizeof(ref_list));
	ref_list.kinds = kinds;
	ref_list.verbose = verbose;
	ref_list.abbrev = abbrev;
	ref_list.with_commit = with_commit;
	if (merge_filter != NO_FILTER)
		init_revisions(&ref_list.revs, NULL);
	cb.ref_list = &ref_list;
	cb.pattern = pattern;
	cb.ret = 0;
	for_each_rawref(append_ref, &cb);
	if (merge_filter != NO_FILTER) {
		struct commit *filter;
		filter = lookup_commit_reference_gently(merge_filter_ref, 0);
		if (!filter)
			die(_("object '%s' does not point to a commit"),
			    sha1_to_hex(merge_filter_ref));

		filter->object.flags |= UNINTERESTING;
		add_pending_object(&ref_list.revs,
				   (struct object *) filter, "");
		ref_list.revs.limited = 1;
		prepare_revision_walk(&ref_list.revs);
		if (verbose)
			ref_list.maxwidth = calc_maxwidth(&ref_list);
	}

	qsort(ref_list.list, ref_list.index, sizeof(struct ref_item), ref_cmp);

	detached = (detached && (kinds & REF_LOCAL_BRANCH));
	if (detached && match_patterns(pattern, "HEAD"))
		show_detached(&ref_list);

	for (i = 0; i < ref_list.index; i++) {
		int current = !detached &&
			(ref_list.list[i].kind == REF_LOCAL_BRANCH) &&
			!strcmp(ref_list.list[i].name, head);
		char *prefix = (kinds != REF_REMOTE_BRANCH &&
				ref_list.list[i].kind == REF_REMOTE_BRANCH)
				? "remotes/" : "";
		print_ref_item(&ref_list.list[i], ref_list.maxwidth, verbose,
			       abbrev, current, prefix);
	}

	free_ref_list(&ref_list);

	if (cb.ret)
		error(_("some refs could not be read"));

	return cb.ret;
}

static void rename_branch(const char *oldname, const char *newname, int force)
{
	struct strbuf oldref = STRBUF_INIT, newref = STRBUF_INIT, logmsg = STRBUF_INIT;
	struct strbuf oldsection = STRBUF_INIT, newsection = STRBUF_INIT;
	int recovery = 0;
	int clobber_head_ok;

	if (!oldname)
		die(_("cannot rename the current branch while not on any."));

	if (strbuf_check_branch_ref(&oldref, oldname)) {
		/*
		 * Bad name --- this could be an attempt to rename a
		 * ref that we used to allow to be created by accident.
		 */
		if (ref_exists(oldref.buf))
			recovery = 1;
		else
			die(_("Invalid branch name: '%s'"), oldname);
	}

	/*
	 * A command like "git branch -M currentbranch currentbranch" cannot
	 * cause the worktree to become inconsistent with HEAD, so allow it.
	 */
	clobber_head_ok = !strcmp(oldname, newname);

	validate_new_branchname(newname, &newref, force, clobber_head_ok);

	strbuf_addf(&logmsg, "Branch: renamed %s to %s",
		 oldref.buf, newref.buf);

	if (rename_ref(oldref.buf, newref.buf, logmsg.buf))
		die(_("Branch rename failed"));
	strbuf_release(&logmsg);

	if (recovery)
		warning(_("Renamed a misnamed branch '%s' away"), oldref.buf + 11);

	/* no need to pass logmsg here as HEAD didn't really move */
	if (!strcmp(oldname, head) && create_symref("HEAD", newref.buf, NULL))
		die(_("Branch renamed to %s, but HEAD is not updated!"), newname);

	strbuf_addf(&oldsection, "branch.%s", oldref.buf + 11);
	strbuf_release(&oldref);
	strbuf_addf(&newsection, "branch.%s", newref.buf + 11);
	strbuf_release(&newref);
	if (git_config_rename_section(oldsection.buf, newsection.buf) < 0)
		die(_("Branch is renamed, but update of config-file failed"));
	strbuf_release(&oldsection);
	strbuf_release(&newsection);
}

static int opt_parse_merge_filter(const struct option *opt, const char *arg, int unset)
{
	merge_filter = ((opt->long_name[0] == 'n')
			? SHOW_NOT_MERGED
			: SHOW_MERGED);
	if (unset)
		merge_filter = SHOW_NOT_MERGED; /* b/c for --no-merged */
	if (!arg)
		arg = "HEAD";
	if (get_sha1(arg, merge_filter_ref))
		die(_("malformed object name %s"), arg);
	return 0;
}

static const char edit_description[] = "BRANCH_DESCRIPTION";

static int edit_branch_description(const char *branch_name)
{
	FILE *fp;
	int status;
	struct strbuf buf = STRBUF_INIT;
	struct strbuf name = STRBUF_INIT;

	read_branch_desc(&buf, branch_name);
	if (!buf.len || buf.buf[buf.len-1] != '\n')
		strbuf_addch(&buf, '\n');
	strbuf_commented_addf(&buf,
		    "Please edit the description for the branch\n"
		    "  %s\n"
		    "Lines starting with '%c' will be stripped.\n",
		    branch_name, comment_line_char);
	fp = fopen(git_path(edit_description), "w");
	if ((fwrite(buf.buf, 1, buf.len, fp) < buf.len) || fclose(fp)) {
		strbuf_release(&buf);
		return error(_("could not write branch description template: %s"),
			     strerror(errno));
	}
	strbuf_reset(&buf);
	if (launch_editor(git_path(edit_description), &buf, NULL)) {
		strbuf_release(&buf);
		return -1;
	}
	stripspace(&buf, 1);

	strbuf_addf(&name, "branch.%s.description", branch_name);
	status = git_config_set(name.buf, buf.len ? buf.buf : NULL);
	strbuf_release(&name);
	strbuf_release(&buf);

	return status;
}

int cmd_branch(int argc, const char **argv, const char *prefix)
{
	int delete = 0, rename = 0, force_create = 0, list = 0;
	int verbose = 0, abbrev = -1, detached = 0;
	int reflog = 0, edit_description = 0;
	int quiet = 0, unset_upstream = 0;
	const char *new_upstream = NULL;
	enum branch_track track;
	int kinds = REF_LOCAL_BRANCH;
	struct commit_list *with_commit = NULL;

	struct option options[] = {
		OPT_GROUP(N_("Generic options")),
		OPT__VERBOSE(&verbose,
			N_("show hash and subject, give twice for upstream branch")),
		OPT__QUIET(&quiet, N_("suppress informational messages")),
		OPT_SET_INT('t', "track",  &track, N_("set up tracking mode (see git-pull(1))"),
			BRANCH_TRACK_EXPLICIT),
		OPT_SET_INT( 0, "set-upstream",  &track, N_("change upstream info"),
			BRANCH_TRACK_OVERRIDE),
		OPT_STRING('u', "set-upstream-to", &new_upstream, "upstream", "change the upstream info"),
		OPT_BOOL(0, "unset-upstream", &unset_upstream, "Unset the upstream info"),
		OPT__COLOR(&branch_use_color, N_("use colored output")),
		OPT_SET_INT('r', "remotes",     &kinds, N_("act on remote-tracking branches"),
			REF_REMOTE_BRANCH),
		{
			OPTION_CALLBACK, 0, "contains", &with_commit, N_("commit"),
			N_("print only branches that contain the commit"),
			PARSE_OPT_LASTARG_DEFAULT,
			parse_opt_with_commit, (intptr_t)"HEAD",
		},
		{
			OPTION_CALLBACK, 0, "with", &with_commit, N_("commit"),
			N_("print only branches that contain the commit"),
			PARSE_OPT_HIDDEN | PARSE_OPT_LASTARG_DEFAULT,
			parse_opt_with_commit, (intptr_t) "HEAD",
		},
		OPT__ABBREV(&abbrev),

		OPT_GROUP(N_("Specific git-branch actions:")),
		OPT_SET_INT('a', "all", &kinds, N_("list both remote-tracking and local branches"),
			REF_REMOTE_BRANCH | REF_LOCAL_BRANCH),
		OPT_BIT('d', "delete", &delete, N_("delete fully merged branch"), 1),
		OPT_BIT('D', NULL, &delete, N_("delete branch (even if not merged)"), 2),
		OPT_BIT('m', "move", &rename, N_("move/rename a branch and its reflog"), 1),
		OPT_BIT('M', NULL, &rename, N_("move/rename a branch, even if target exists"), 2),
		OPT_BOOL(0, "list", &list, N_("list branch names")),
		OPT_BOOL('l', "create-reflog", &reflog, N_("create the branch's reflog")),
		OPT_BOOL(0, "edit-description", &edit_description,
			 N_("edit the description for the branch")),
		OPT__FORCE(&force_create, N_("force creation (when already exists)")),
		{
			OPTION_CALLBACK, 0, "no-merged", &merge_filter_ref,
			N_("commit"), N_("print only not merged branches"),
			PARSE_OPT_LASTARG_DEFAULT | PARSE_OPT_NONEG,
			opt_parse_merge_filter, (intptr_t) "HEAD",
		},
		{
			OPTION_CALLBACK, 0, "merged", &merge_filter_ref,
			N_("commit"), N_("print only merged branches"),
			PARSE_OPT_LASTARG_DEFAULT | PARSE_OPT_NONEG,
			opt_parse_merge_filter, (intptr_t) "HEAD",
		},
		OPT_COLUMN(0, "column", &colopts, N_("list branches in columns")),
		OPT_END(),
	};

	if (argc == 2 && !strcmp(argv[1], "-h"))
		usage_with_options(builtin_branch_usage, options);

	git_config(git_branch_config, NULL);

	track = git_branch_track;

	head = resolve_refdup("HEAD", head_sha1, 0, NULL);
	if (!head)
		die(_("Failed to resolve HEAD as a valid ref."));
	if (!strcmp(head, "HEAD")) {
		detached = 1;
	} else {
		if (!starts_with(head, "refs/heads/"))
			die(_("HEAD not found below refs/heads!"));
		head += 11;
	}
	hashcpy(merge_filter_ref, head_sha1);


	argc = parse_options(argc, argv, prefix, options, builtin_branch_usage,
			     0);

	if (!delete && !rename && !edit_description && !new_upstream && !unset_upstream && argc == 0)
		list = 1;

	if (with_commit || merge_filter != NO_FILTER)
		list = 1;

	if (!!delete + !!rename + !!force_create + !!new_upstream +
	    list + unset_upstream > 1)
		usage_with_options(builtin_branch_usage, options);

	if (abbrev == -1)
		abbrev = DEFAULT_ABBREV;
	finalize_colopts(&colopts, -1);
	if (verbose) {
		if (explicitly_enable_column(colopts))
			die(_("--column and --verbose are incompatible"));
		colopts = 0;
	}

	if (delete) {
		if (!argc)
			die(_("branch name required"));
		return delete_branches(argc, argv, delete > 1, kinds, quiet);
	} else if (list) {
		int ret = print_ref_list(kinds, detached, verbose, abbrev,
					 with_commit, argv);
		print_columns(&output, colopts, NULL);
		string_list_clear(&output, 0);
		return ret;
	}
	else if (edit_description) {
		const char *branch_name;
		struct strbuf branch_ref = STRBUF_INIT;

		if (!argc) {
			if (detached)
				die(_("Cannot give description to detached HEAD"));
			branch_name = head;
		} else if (argc == 1)
			branch_name = argv[0];
		else
			die(_("cannot edit description of more than one branch"));

		strbuf_addf(&branch_ref, "refs/heads/%s", branch_name);
		if (!ref_exists(branch_ref.buf)) {
			strbuf_release(&branch_ref);

			if (!argc)
				return error(_("No commit on branch '%s' yet."),
					     branch_name);
			else
				return error(_("No branch named '%s'."),
					     branch_name);
		}
		strbuf_release(&branch_ref);

		if (edit_branch_description(branch_name))
			return 1;
	} else if (rename) {
		if (!argc)
			die(_("branch name required"));
		else if (argc == 1)
			rename_branch(head, argv[0], rename > 1);
		else if (argc == 2)
			rename_branch(argv[0], argv[1], rename > 1);
		else
			die(_("too many branches for a rename operation"));
	} else if (new_upstream) {
		struct branch *branch = branch_get(argv[0]);

		if (argc > 1)
			die(_("too many branches to set new upstream"));

		if (!branch) {
			if (!argc || !strcmp(argv[0], "HEAD"))
				die(_("could not set upstream of HEAD to %s when "
				      "it does not point to any branch."),
				    new_upstream);
			die(_("no such branch '%s'"), argv[0]);
		}

		if (!ref_exists(branch->refname))
			die(_("branch '%s' does not exist"), branch->name);

		/*
		 * create_branch takes care of setting up the tracking
		 * info and making sure new_upstream is correct
		 */
		create_branch(head, branch->name, new_upstream, 0, 0, 0, quiet, BRANCH_TRACK_OVERRIDE);
	} else if (unset_upstream) {
		struct branch *branch = branch_get(argv[0]);
		struct strbuf buf = STRBUF_INIT;

		if (argc > 1)
			die(_("too many branches to unset upstream"));

		if (!branch) {
			if (!argc || !strcmp(argv[0], "HEAD"))
				die(_("could not unset upstream of HEAD when "
				      "it does not point to any branch."));
			die(_("no such branch '%s'"), argv[0]);
		}

		if (!branch_has_merge_config(branch))
			die(_("Branch '%s' has no upstream information"), branch->name);

		strbuf_addf(&buf, "branch.%s.remote", branch->name);
		git_config_set_multivar(buf.buf, NULL, NULL, 1);
		strbuf_reset(&buf);
		strbuf_addf(&buf, "branch.%s.merge", branch->name);
		git_config_set_multivar(buf.buf, NULL, NULL, 1);
		strbuf_release(&buf);
	} else if (argc > 0 && argc <= 2) {
		struct branch *branch = branch_get(argv[0]);
		int branch_existed = 0, remote_tracking = 0;
		struct strbuf buf = STRBUF_INIT;

		if (!strcmp(argv[0], "HEAD"))
			die(_("it does not make sense to create 'HEAD' manually"));

		if (!branch)
			die(_("no such branch '%s'"), argv[0]);

		if (kinds != REF_LOCAL_BRANCH)
			die(_("-a and -r options to 'git branch' do not make sense with a branch name"));

		if (track == BRANCH_TRACK_OVERRIDE)
			fprintf(stderr, _("The --set-upstream flag is deprecated and will be removed. Consider using --track or --set-upstream-to\n"));

		strbuf_addf(&buf, "refs/remotes/%s", branch->name);
		remote_tracking = ref_exists(buf.buf);
		strbuf_release(&buf);

		branch_existed = ref_exists(branch->refname);
		create_branch(head, argv[0], (argc == 2) ? argv[1] : head,
			      force_create, reflog, 0, quiet, track);

		/*
		 * We only show the instructions if the user gave us
		 * one branch which doesn't exist locally, but is the
		 * name of a remote-tracking branch.
		 */
		if (argc == 1 && track == BRANCH_TRACK_OVERRIDE &&
		    !branch_existed && remote_tracking) {
			fprintf(stderr, _("\nIf you wanted to make '%s' track '%s', do this:\n\n"), head, branch->name);
			fprintf(stderr, _("    git branch -d %s\n"), branch->name);
			fprintf(stderr, _("    git branch --set-upstream-to %s\n"), branch->name);
		}

	} else
		usage_with_options(builtin_branch_usage, options);

	return 0;
}
