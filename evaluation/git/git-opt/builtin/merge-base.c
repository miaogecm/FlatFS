#include "builtin.h"
#include "cache.h"
#include "commit.h"
#include "refs.h"
#include "diff.h"
#include "revision.h"
#include "parse-options.h"

static int show_merge_base(struct commit **rev, int rev_nr, int show_all)
{
	struct commit_list *result;

	result = get_merge_bases_many(rev[0], rev_nr - 1, rev + 1, 0);

	if (!result)
		return 1;

	while (result) {
		printf("%s\n", sha1_to_hex(result->item->object.sha1));
		if (!show_all)
			return 0;
		result = result->next;
	}

	return 0;
}

static const char * const merge_base_usage[] = {
	N_("git merge-base [-a|--all] <commit> <commit>..."),
	N_("git merge-base [-a|--all] --octopus <commit>..."),
	N_("git merge-base --independent <commit>..."),
	N_("git merge-base --is-ancestor <commit> <commit>"),
	N_("git merge-base --fork-point <ref> [<commit>]"),
	NULL
};

static struct commit *get_commit_reference(const char *arg)
{
	unsigned char revkey[20];
	struct commit *r;

	if (get_sha1(arg, revkey))
		die("Not a valid object name %s", arg);
	r = lookup_commit_reference(revkey);
	if (!r)
		die("Not a valid commit name %s", arg);

	return r;
}

static int handle_independent(int count, const char **args)
{
	struct commit_list *revs = NULL;
	struct commit_list *result;
	int i;

	for (i = count - 1; i >= 0; i--)
		commit_list_insert(get_commit_reference(args[i]), &revs);

	result = reduce_heads(revs);
	if (!result)
		return 1;

	while (result) {
		printf("%s\n", sha1_to_hex(result->item->object.sha1));
		result = result->next;
	}
	return 0;
}

static int handle_octopus(int count, const char **args, int show_all)
{
	struct commit_list *revs = NULL;
	struct commit_list *result;
	int i;

	for (i = count - 1; i >= 0; i--)
		commit_list_insert(get_commit_reference(args[i]), &revs);

	result = reduce_heads(get_octopus_merge_bases(revs));

	if (!result)
		return 1;

	while (result) {
		printf("%s\n", sha1_to_hex(result->item->object.sha1));
		if (!show_all)
			return 0;
		result = result->next;
	}

	return 0;
}

static int handle_is_ancestor(int argc, const char **argv)
{
	struct commit *one, *two;

	if (argc != 2)
		die("--is-ancestor takes exactly two commits");
	one = get_commit_reference(argv[0]);
	two = get_commit_reference(argv[1]);
	if (in_merge_bases(one, two))
		return 0;
	else
		return 1;
}

struct rev_collect {
	struct commit **commit;
	int nr;
	int alloc;
	unsigned int initial : 1;
};

static void add_one_commit(unsigned char *sha1, struct rev_collect *revs)
{
	struct commit *commit;

	if (is_null_sha1(sha1))
		return;

	commit = lookup_commit(sha1);
	if (!commit ||
	    (commit->object.flags & TMP_MARK) ||
	    parse_commit(commit))
		return;

	ALLOC_GROW(revs->commit, revs->nr + 1, revs->alloc);
	revs->commit[revs->nr++] = commit;
	commit->object.flags |= TMP_MARK;
}

static int collect_one_reflog_ent(unsigned char *osha1, unsigned char *nsha1,
				  const char *ident, unsigned long timestamp,
				  int tz, const char *message, void *cbdata)
{
	struct rev_collect *revs = cbdata;

	if (revs->initial) {
		revs->initial = 0;
		add_one_commit(osha1, revs);
	}
	add_one_commit(nsha1, revs);
	return 0;
}

static int handle_fork_point(int argc, const char **argv)
{
	unsigned char sha1[20];
	char *refname;
	const char *commitname;
	struct rev_collect revs;
	struct commit *derived;
	struct commit_list *bases;
	int i, ret = 0;

	switch (dwim_ref(argv[0], strlen(argv[0]), sha1, &refname)) {
	case 0:
		die("No such ref: '%s'", argv[0]);
	case 1:
		break; /* good */
	default:
		die("Ambiguous refname: '%s'", argv[0]);
	}

	commitname = (argc == 2) ? argv[1] : "HEAD";
	if (get_sha1(commitname, sha1))
		die("Not a valid object name: '%s'", commitname);

	derived = lookup_commit_reference(sha1);
	memset(&revs, 0, sizeof(revs));
	revs.initial = 1;
	for_each_reflog_ent(refname, collect_one_reflog_ent, &revs);

	for (i = 0; i < revs.nr; i++)
		revs.commit[i]->object.flags &= ~TMP_MARK;

	bases = get_merge_bases_many(derived, revs.nr, revs.commit, 0);

	/*
	 * There should be one and only one merge base, when we found
	 * a common ancestor among reflog entries.
	 */
	if (!bases || bases->next) {
		ret = 1;
		goto cleanup_return;
	}

	/* And the found one must be one of the reflog entries */
	for (i = 0; i < revs.nr; i++)
		if (&bases->item->object == &revs.commit[i]->object)
			break; /* found */
	if (revs.nr <= i) {
		ret = 1; /* not found */
		goto cleanup_return;
	}

	printf("%s\n", sha1_to_hex(bases->item->object.sha1));

cleanup_return:
	free_commit_list(bases);
	return ret;
}

int cmd_merge_base(int argc, const char **argv, const char *prefix)
{
	struct commit **rev;
	int rev_nr = 0;
	int show_all = 0;
	int cmdmode = 0;

	struct option options[] = {
		OPT_BOOL('a', "all", &show_all, N_("output all common ancestors")),
		OPT_CMDMODE(0, "octopus", &cmdmode,
			    N_("find ancestors for a single n-way merge"), 'o'),
		OPT_CMDMODE(0, "independent", &cmdmode,
			    N_("list revs not reachable from others"), 'r'),
		OPT_CMDMODE(0, "is-ancestor", &cmdmode,
			    N_("is the first one ancestor of the other?"), 'a'),
		OPT_CMDMODE(0, "fork-point", &cmdmode,
			    N_("find where <commit> forked from reflog of <ref>"), 'f'),
		OPT_END()
	};

	git_config(git_default_config, NULL);
	argc = parse_options(argc, argv, prefix, options, merge_base_usage, 0);

	if (cmdmode == 'a') {
		if (argc < 2)
			usage_with_options(merge_base_usage, options);
		if (show_all)
			die("--is-ancestor cannot be used with --all");
		return handle_is_ancestor(argc, argv);
	}

	if (cmdmode == 'r' && show_all)
		die("--independent cannot be used with --all");

	if (cmdmode == 'o')
		return handle_octopus(argc, argv, show_all);

	if (cmdmode == 'r')
		return handle_independent(argc, argv);

	if (cmdmode == 'f') {
		if (argc < 1 || 2 < argc)
			usage_with_options(merge_base_usage, options);
		return handle_fork_point(argc, argv);
	}

	if (argc < 2)
		usage_with_options(merge_base_usage, options);

	rev = xmalloc(argc * sizeof(*rev));
	while (argc-- > 0)
		rev[rev_nr++] = get_commit_reference(*argv++);
	return show_merge_base(rev, rev_nr, show_all);
}
