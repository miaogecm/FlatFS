/*
 * name-hash.c
 *
 * Hashing names in the index state
 *
 * Copyright (C) 2008 Linus Torvalds
 */
#define NO_THE_INDEX_COMPATIBILITY_MACROS
#include "cache.h"

/*
 * This removes bit 5 if bit 6 is set.
 *
 * That will make US-ASCII characters hash to their upper-case
 * equivalent. We could easily do this one whole word at a time,
 * but that's for future worries.
 */
static inline unsigned char icase_hash(unsigned char c)
{
	return c & ~((c & 0x40) >> 1);
}

static unsigned int hash_name(const char *name, int namelen)
{
	unsigned int hash = 0x123;

	while (namelen--) {
		unsigned char c = *name++;
		c = icase_hash(c);
		hash = hash*101 + c;
	}
	return hash;
}

struct dir_entry {
	struct dir_entry *next;
	struct dir_entry *parent;
	struct cache_entry *ce;
	int nr;
	unsigned int namelen;
};

static struct dir_entry *find_dir_entry(struct index_state *istate,
		const char *name, unsigned int namelen)
{
	unsigned int hash = hash_name(name, namelen);
	struct dir_entry *dir;

	for (dir = lookup_hash(hash, &istate->dir_hash); dir; dir = dir->next)
		if (dir->namelen == namelen &&
		    !strncasecmp(dir->ce->name, name, namelen))
			return dir;
	return NULL;
}

static struct dir_entry *hash_dir_entry(struct index_state *istate,
		struct cache_entry *ce, int namelen)
{
	/*
	 * Throw each directory component in the hash for quick lookup
	 * during a git status. Directory components are stored without their
	 * closing slash.  Despite submodules being a directory, they never
	 * reach this point, because they are stored
	 * in index_state.name_hash (as ordinary cache_entries).
	 *
	 * Note that the cache_entry stored with the dir_entry merely
	 * supplies the name of the directory (up to dir_entry.namelen). We
	 * track the number of 'active' files in a directory in dir_entry.nr,
	 * so we can tell if the directory is still relevant, e.g. for git
	 * status. However, if cache_entries are removed, we cannot pinpoint
	 * an exact cache_entry that's still active. It is very possible that
	 * multiple dir_entries point to the same cache_entry.
	 */
	struct dir_entry *dir;

	/* get length of parent directory */
	while (namelen > 0 && !is_dir_sep(ce->name[namelen - 1]))
		namelen--;
	if (namelen <= 0)
		return NULL;
	namelen--;

	/* lookup existing entry for that directory */
	dir = find_dir_entry(istate, ce->name, namelen);
	if (!dir) {
		/* not found, create it and add to hash table */
		void **pdir;
		unsigned int hash = hash_name(ce->name, namelen);

		dir = xcalloc(1, sizeof(struct dir_entry));
		dir->namelen = namelen;
		dir->ce = ce;

		pdir = insert_hash(hash, dir, &istate->dir_hash);
		if (pdir) {
			dir->next = *pdir;
			*pdir = dir;
		}

		/* recursively add missing parent directories */
		dir->parent = hash_dir_entry(istate, ce, namelen);
	}
	return dir;
}

static void add_dir_entry(struct index_state *istate, struct cache_entry *ce)
{
	/* Add reference to the directory entry (and parents if 0). */
	struct dir_entry *dir = hash_dir_entry(istate, ce, ce_namelen(ce));
	while (dir && !(dir->nr++))
		dir = dir->parent;
}

static void remove_dir_entry(struct index_state *istate, struct cache_entry *ce)
{
	/*
	 * Release reference to the directory entry (and parents if 0).
	 *
	 * Note: we do not remove / free the entry because there's no
	 * hash.[ch]::remove_hash and dir->next may point to other entries
	 * that are still valid, so we must not free the memory.
	 */
	struct dir_entry *dir = hash_dir_entry(istate, ce, ce_namelen(ce));
	while (dir && dir->nr && !(--dir->nr))
		dir = dir->parent;
}

static void hash_index_entry(struct index_state *istate, struct cache_entry *ce)
{
	void **pos;
	unsigned int hash;

	if (ce->ce_flags & CE_HASHED)
		return;
	ce->ce_flags |= CE_HASHED;
	ce->next = NULL;
	hash = hash_name(ce->name, ce_namelen(ce));
	pos = insert_hash(hash, ce, &istate->name_hash);
	if (pos) {
		ce->next = *pos;
		*pos = ce;
	}

	if (ignore_case && !(ce->ce_flags & CE_UNHASHED))
		add_dir_entry(istate, ce);
}

static void lazy_init_name_hash(struct index_state *istate)
{
	int nr;

	if (istate->name_hash_initialized)
		return;
	if (istate->cache_nr)
		preallocate_hash(&istate->name_hash, istate->cache_nr);
	for (nr = 0; nr < istate->cache_nr; nr++)
		hash_index_entry(istate, istate->cache[nr]);
	istate->name_hash_initialized = 1;
}

void add_name_hash(struct index_state *istate, struct cache_entry *ce)
{
	/* if already hashed, add reference to directory entries */
	if (ignore_case && (ce->ce_flags & CE_STATE_MASK) == CE_STATE_MASK)
		add_dir_entry(istate, ce);

	ce->ce_flags &= ~CE_UNHASHED;
	if (istate->name_hash_initialized)
		hash_index_entry(istate, ce);
}

/*
 * We don't actually *remove* it, we can just mark it invalid so that
 * we won't find it in lookups.
 *
 * Not only would we have to search the lists (simple enough), but
 * we'd also have to rehash other hash buckets in case this makes the
 * hash bucket empty (common). So it's much better to just mark
 * it.
 */
void remove_name_hash(struct index_state *istate, struct cache_entry *ce)
{
	/* if already hashed, release reference to directory entries */
	if (ignore_case && (ce->ce_flags & CE_STATE_MASK) == CE_HASHED)
		remove_dir_entry(istate, ce);

	ce->ce_flags |= CE_UNHASHED;
}

static int slow_same_name(const char *name1, int len1, const char *name2, int len2)
{
	if (len1 != len2)
		return 0;

	while (len1) {
		unsigned char c1 = *name1++;
		unsigned char c2 = *name2++;
		len1--;
		if (c1 != c2) {
			c1 = toupper(c1);
			c2 = toupper(c2);
			if (c1 != c2)
				return 0;
		}
	}
	return 1;
}

static int same_name(const struct cache_entry *ce, const char *name, int namelen, int icase)
{
	int len = ce_namelen(ce);

	/*
	 * Always do exact compare, even if we want a case-ignoring comparison;
	 * we do the quick exact one first, because it will be the common case.
	 */
	if (len == namelen && !cache_name_compare(name, namelen, ce->name, len))
		return 1;

	if (!icase)
		return 0;

	return slow_same_name(name, namelen, ce->name, len);
}

struct cache_entry *index_dir_exists(struct index_state *istate, const char *name, int namelen)
{
	struct cache_entry *ce;
	struct dir_entry *dir;

	lazy_init_name_hash(istate);
	dir = find_dir_entry(istate, name, namelen);
	if (dir && dir->nr)
		return dir->ce;

	/*
	 * It might be a submodule. Unlike plain directories, which are stored
	 * in the dir-hash, submodules are stored in the name-hash, so check
	 * there, as well.
	 */
	ce = index_file_exists(istate, name, namelen, 1);
	if (ce && S_ISGITLINK(ce->ce_mode))
		return ce;

	return NULL;
}

struct cache_entry *index_file_exists(struct index_state *istate, const char *name, int namelen, int icase)
{
	unsigned int hash = hash_name(name, namelen);
	struct cache_entry *ce;

	lazy_init_name_hash(istate);
	ce = lookup_hash(hash, &istate->name_hash);

	while (ce) {
		if (!(ce->ce_flags & CE_UNHASHED)) {
			if (same_name(ce, name, namelen, icase))
				return ce;
		}
		ce = ce->next;
	}
	return NULL;
}

struct cache_entry *index_name_exists(struct index_state *istate, const char *name, int namelen, int icase)
{
	if (namelen > 0 && name[namelen - 1] == '/')
		return index_dir_exists(istate, name, namelen - 1);
	return index_file_exists(istate, name, namelen, icase);
}

static int free_dir_entry(void *entry, void *unused)
{
	struct dir_entry *dir = entry;
	while (dir) {
		struct dir_entry *next = dir->next;
		free(dir);
		dir = next;
	}
	return 0;
}

void free_name_hash(struct index_state *istate)
{
	if (!istate->name_hash_initialized)
		return;
	istate->name_hash_initialized = 0;
	if (ignore_case)
		/* free directory entries */
		for_each_hash(&istate->dir_hash, free_dir_entry, NULL);

	free_hash(&istate->name_hash);
	free_hash(&istate->dir_hash);
}
