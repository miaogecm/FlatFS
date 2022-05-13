/*
 * We put all the git config variables in this same object
 * file, so that programs can link against the config parser
 * without having to link against all the rest of git.
 *
 * In particular, no need to bring in libz etc unless needed,
 * even if you might want to know where the git directory etc
 * are.
 */
#include "cache.h"
#include "refs.h"
#include "fmt-merge-msg.h"
#include "commit.h"

int trust_executable_bit = 1;
int trust_ctime = 1;
int check_stat = 1;
int has_symlinks = 1;
int minimum_abbrev = 4, default_abbrev = 7;
int ignore_case;
int assume_unchanged;
int prefer_symlink_refs;
int is_bare_repository_cfg = -1; /* unspecified */
int log_all_ref_updates = -1; /* unspecified */
int warn_ambiguous_refs = 1;
int warn_on_object_refname_ambiguity = 1;
int repository_format_version;
const char *git_commit_encoding;
const char *git_log_output_encoding;
int shared_repository = PERM_UMASK;
const char *apply_default_whitespace;
const char *apply_default_ignorewhitespace;
const char *git_attributes_file;
int zlib_compression_level = Z_BEST_SPEED;
int core_compression_level;
int core_compression_seen;
int fsync_object_files;
size_t packed_git_window_size = DEFAULT_PACKED_GIT_WINDOW_SIZE;
size_t packed_git_limit = DEFAULT_PACKED_GIT_LIMIT;
size_t delta_base_cache_limit = 16 * 1024 * 1024;
unsigned long big_file_threshold = 512 * 1024 * 1024;
const char *pager_program;
int pager_use_color = 1;
const char *editor_program;
const char *askpass_program;
const char *excludes_file;
enum auto_crlf auto_crlf = AUTO_CRLF_FALSE;
int read_replace_refs = 1; /* NEEDSWORK: rename to use_replace_refs */
enum eol core_eol = EOL_UNSET;
enum safe_crlf safe_crlf = SAFE_CRLF_WARN;
unsigned whitespace_rule_cfg = WS_DEFAULT_RULE;
enum branch_track git_branch_track = BRANCH_TRACK_REMOTE;
enum rebase_setup_type autorebase = AUTOREBASE_NEVER;
enum push_default_type push_default = PUSH_DEFAULT_UNSPECIFIED;
#ifndef OBJECT_CREATION_MODE
#define OBJECT_CREATION_MODE OBJECT_CREATION_USES_HARDLINKS
#endif
enum object_creation_mode object_creation_mode = OBJECT_CREATION_MODE;
char *notes_ref_name;
int grafts_replace_parents = 1;
int core_apply_sparse_checkout;
int merge_log_config = -1;
int precomposed_unicode = -1; /* see probe_utf8_pathname_composition() */
struct startup_info *startup_info;
unsigned long pack_size_limit_cfg;

/*
 * The character that begins a commented line in user-editable file
 * that is subject to stripspace.
 */
char comment_line_char = '#';

/* Parallel index stat data preload? */
int core_preload_index = 0;

/* This is set by setup_git_dir_gently() and/or git_default_config() */
char *git_work_tree_cfg;
static char *work_tree;

static const char *namespace;
static size_t namespace_len;

static const char *git_dir;
static char *git_object_dir, *git_index_file, *git_graft_file;

/*
 * Repository-local GIT_* environment variables; see cache.h for details.
 */
const char * const local_repo_env[] = {
	ALTERNATE_DB_ENVIRONMENT,
	CONFIG_ENVIRONMENT,
	CONFIG_DATA_ENVIRONMENT,
	DB_ENVIRONMENT,
	GIT_DIR_ENVIRONMENT,
	GIT_WORK_TREE_ENVIRONMENT,
	GIT_IMPLICIT_WORK_TREE_ENVIRONMENT,
	GRAFT_ENVIRONMENT,
	INDEX_ENVIRONMENT,
	NO_REPLACE_OBJECTS_ENVIRONMENT,
	GIT_PREFIX_ENVIRONMENT,
	GIT_SHALLOW_FILE_ENVIRONMENT,
	NULL
};

static char *expand_namespace(const char *raw_namespace)
{
	struct strbuf buf = STRBUF_INIT;
	struct strbuf **components, **c;

	if (!raw_namespace || !*raw_namespace)
		return xstrdup("");

	strbuf_addstr(&buf, raw_namespace);
	components = strbuf_split(&buf, '/');
	strbuf_reset(&buf);
	for (c = components; *c; c++)
		if (strcmp((*c)->buf, "/") != 0)
			strbuf_addf(&buf, "refs/namespaces/%s", (*c)->buf);
	strbuf_list_free(components);
	if (check_refname_format(buf.buf, 0))
		die("bad git namespace path \"%s\"", raw_namespace);
	strbuf_addch(&buf, '/');
	return strbuf_detach(&buf, NULL);
}

static void setup_git_env(void)
{
	const char *gitfile;
	const char *shallow_file;

	git_dir = getenv(GIT_DIR_ENVIRONMENT);
	if (!git_dir)
		git_dir = DEFAULT_GIT_DIR_ENVIRONMENT;
	gitfile = read_gitfile(git_dir);
	git_dir = xstrdup(gitfile ? gitfile : git_dir);
	git_object_dir = getenv(DB_ENVIRONMENT);
	if (!git_object_dir) {
		git_object_dir = xmalloc(strlen(git_dir) + 9);
		sprintf(git_object_dir, "%s/objects", git_dir);
	}
	git_index_file = getenv(INDEX_ENVIRONMENT);
	if (!git_index_file) {
		git_index_file = xmalloc(strlen(git_dir) + 7);
		sprintf(git_index_file, "%s/index", git_dir);
	}
	git_graft_file = getenv(GRAFT_ENVIRONMENT);
	if (!git_graft_file)
		git_graft_file = git_pathdup("info/grafts");
	if (getenv(NO_REPLACE_OBJECTS_ENVIRONMENT))
		read_replace_refs = 0;
	namespace = expand_namespace(getenv(GIT_NAMESPACE_ENVIRONMENT));
	namespace_len = strlen(namespace);
	shallow_file = getenv(GIT_SHALLOW_FILE_ENVIRONMENT);
	if (shallow_file)
		set_alternate_shallow_file(shallow_file, 0);
}

int is_bare_repository(void)
{
	/* if core.bare is not 'false', let's see if there is a work tree */
	return is_bare_repository_cfg && !get_git_work_tree();
}

const char *get_git_dir(void)
{
	if (!git_dir)
		setup_git_env();
	return git_dir;
}

const char *get_git_namespace(void)
{
	if (!namespace)
		setup_git_env();
	return namespace;
}

const char *strip_namespace(const char *namespaced_ref)
{
	if (!starts_with(namespaced_ref, get_git_namespace()))
		return NULL;
	return namespaced_ref + namespace_len;
}

static int git_work_tree_initialized;

/*
 * Note.  This works only before you used a work tree.  This was added
 * primarily to support git-clone to work in a new repository it just
 * created, and is not meant to flip between different work trees.
 */
void set_git_work_tree(const char *new_work_tree)
{
	if (git_work_tree_initialized) {
		new_work_tree = real_path(new_work_tree);
		if (strcmp(new_work_tree, work_tree))
			die("internal error: work tree has already been set\n"
			    "Current worktree: %s\nNew worktree: %s",
			    work_tree, new_work_tree);
		return;
	}
	git_work_tree_initialized = 1;
	work_tree = xstrdup(real_path(new_work_tree));
}

const char *get_git_work_tree(void)
{
	return work_tree;
}

char *get_object_directory(void)
{
	if (!git_object_dir)
		setup_git_env();
	return git_object_dir;
}

int odb_mkstemp(char *template, size_t limit, const char *pattern)
{
	int fd;
	/*
	 * we let the umask do its job, don't try to be more
	 * restrictive except to remove write permission.
	 */
	int mode = 0444;
	snprintf(template, limit, "%s/%s",
		 get_object_directory(), pattern);
	fd = git_mkstemp_mode(template, mode);
	if (0 <= fd)
		return fd;

	/* slow path */
	/* some mkstemp implementations erase template on failure */
	snprintf(template, limit, "%s/%s",
		 get_object_directory(), pattern);
	safe_create_leading_directories(template);
	return xmkstemp_mode(template, mode);
}

int odb_pack_keep(char *name, size_t namesz, unsigned char *sha1)
{
	int fd;

	snprintf(name, namesz, "%s/pack/pack-%s.keep",
		 get_object_directory(), sha1_to_hex(sha1));
	fd = open(name, O_RDWR|O_CREAT|O_EXCL, 0600);
	if (0 <= fd)
		return fd;

	/* slow path */
	safe_create_leading_directories(name);
	return open(name, O_RDWR|O_CREAT|O_EXCL, 0600);
}

char *get_index_file(void)
{
	if (!git_index_file)
		setup_git_env();
	return git_index_file;
}

char *get_graft_file(void)
{
	if (!git_graft_file)
		setup_git_env();
	return git_graft_file;
}

int set_git_dir(const char *path)
{
	if (setenv(GIT_DIR_ENVIRONMENT, path, 1))
		return error("Could not set GIT_DIR to '%s'", path);
	setup_git_env();
	return 0;
}

const char *get_log_output_encoding(void)
{
	return git_log_output_encoding ? git_log_output_encoding
		: get_commit_output_encoding();
}

const char *get_commit_output_encoding(void)
{
	return git_commit_encoding ? git_commit_encoding : "UTF-8";
}
