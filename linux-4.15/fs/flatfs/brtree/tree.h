/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * BrTree: Range-optimized B+ Tree
 */

#ifndef BRTREE_H
#define BRTREE_H

#include <linux/kernel.h>
#include "../flatfs_def.h"

/* No offset */
#define BRT_NOFF            0

/* Maximum tree height */
#define BRT_MAX_HEIGHT      8

/* The order of B+ Tree */
#define BRT_DEGREE          21
#define BRT_MAX_NR_ENT      (BRT_DEGREE * 2 - 1)
#define BRT_MIN_NR_KEY      (BRT_DEGREE - 1)

/* Segmentation configration */
#define BRT_SEG_SIZE        40
#define BRT_IENT_NR_SEG     1
#define BRT_DENT_NR_SEG     2

/* The node prefix buffer size */
#define BRT_ND_PRE_BUF_SIZE 1024

/* Max prefetch size when prefetching from prefix to suffix */
#define BRT_PREFETCH_SIZE   32ul  // 32 B

struct brt_tree_s;
struct brt_inode_s;
struct brt_dnode_s;

struct sok_pool;

/* the BrTree iterator, used for range query */
struct brt_it_s {
    brt_tree_t *tree;
    fastr_t pathname, end;
    struct sok_pool *cur;
    int cur_pos, end_pos;
};

/* statistics of a BrTree */
struct brt_stat_s {
    int height;
    int nr_nodes;
    int nr_entries;
    int nr_filename[BRT_DENT_NR_SEG + 1][2];
    int nr_suf[BRT_DENT_NR_SEG + 1][2];
    int nr_suf_cached[BRT_DENT_NR_SEG + 1][2];
    size_t total_filename_len;
    size_t total_suf_len;
    size_t total_suf_cached_len;
    size_t total_path_len;
};

void brt_init(struct super_block *sb);
void brt_deinit(struct super_block *sb);

/***************************************/
/*          BrTree operations          */
/***************************************/

/*
 * Generate the corresponding in-DRAM Br-Tree @dst from
 * persistent Br-Tree @src.
 */
void brt_pin(brt_tree_t *dst, brt_ptree_t *src,
             struct flatfs_sb_info *sbi);
void brt_set_main(brt_tree_t *tree);
void brt_unset_main(brt_tree_t *tree);

/*
 * Lookup an entry in BrTree, returns 0 or -EAGAIN for a successful
 * lookup, or -ENOENT if not found. If succeeded, @res will be set.
 * And remember to call @brt_got after using @res to free resources.
 *
 * If res->mode is BRT_QR_SEMANTIC, then -EAGAIN may be returned to
 * indicate that the path walk is only partially done because of a
 * symbolic link or mount point. The completed part is stored inside
 * res->path, which is just a slice of @key rather than a copy
 * of @key's substring.
 */
int brt_get(brt_tree_t *tree, brt_qr_t *res, brt_key_t key);
void brt_got(brt_tree_t *tree, brt_qr_t *res);

/*
 * Add an entry to BrTree.
 *
 * @pppcs and @pppc specifies the parent directory's prefix permission
 * string and its own prefix permission compressor (depth=1).
 *
 * Returns 0 if succeeded. Returns ino if the key exists in BrTree.
 *
 * [Atomicity] @brt_add is atomic.
 */
long brt_add(brt_tree_t *tree,
             brt_key_t k, brt_val_t v,
             ppcs_t pppcs, ppc_t pppc);
int brt_upd(brt_tree_t *tree, brt_key_t k, brt_val_t v);

/*
 * Remove an entry from BrTree.
 *
 * Returns 0 if succeeded. Returns -EEXIST if not found.
 *
 * [Atomicity] @brt_del is atomic.
 */
int brt_del(brt_tree_t* tree, brt_key_t key);

/*
 * Duplicate @src to @dst. @dst must be an empty BrTree. @dst's @ptree
 * must be set before invocation.
 *
 * IMPORTANT NOTE: Ensure no concurrent modification of @src during
 * invocation.
 *
 * [Atomicity] @brt_dup is atomic, which means that either the whole
 * @src is copied to @dst, or the @dst still remains empty.
 */
void brt_dup(brt_tree_t *dst, brt_tree_t *src);

/*
 * Clear the BrTree, make it empty, and free all the nodes and entries.
 *
 * IMPORTANT NOTE: Ensure no concurrent modification of @tree during
 * invocation.
 *
 * [Atomicity] @brt_clear is atomic. @tree is bound to be cleared.
 */
void brt_clear(brt_tree_t *tree);

/*
 * Create an iterator starting from the first entry whose key is equal
 * to or greater than @lowerbound. You may use @brt_it_next or @brt_it_close
 * to manipulate the iterator.
 */
void brt_scan(brt_tree_t *tree, brt_it_t *it, brt_key_t start, brt_key_t end);

/*
 * Let @dst absorb @src. If succeeded, the @src becomes an empty tree.
 *
 * IMPORTANT NOTE: Ensure no concurrent modification of @src, @dst during
 * invocation.
 *
 * [Atomicity] @brt_absorb is atomic, which means that either @src becomes
 * an empty tree and @dst becomes the union of @dst and @src, or @dst,@src
 * remains unchanged.
 */
int brt_absorb(brt_tree_t *dst, brt_tree_t *src);

/*
 * Let @src releases all its entries whose values are equal to or greater
 * than @lobnd to @dst. The @dst must be an empty BrTree before invocation.
 *
 * IMPORTANT NOTE: Ensure no concurrent modification of @src, @dst during
 * invocation.
 *
 * [Atomicity] @brt_release is atomic, which means that either @dst contains
 * all the entries that are equal to or greater than @lowerbound, and @src contains
 * none of them, or @dst,@src remains unchanged.
 */
int brt_release(brt_tree_t *dst, brt_tree_t *src, brt_key_t lobnd);

/* TODO: Add comment */
void brt_chgpre(brt_tree_t *tree, brt_chgpre_opt_t *opt);

/***************************************/
/*          Iterator operations        */
/***************************************/

/*
 * Get next range query result. The result will be put into @ent.
 * Access the @ent as quick as possible, because the corresponding
 * node will be locked before the cursor points to the next node.
 *
 * Returns 0 if succeeded, -ENOENT if there's no next entry.
 *
 * The @path field of @ent is read-only, do not modify it directly.
 *
 * Do not forget to call @brt_it_close right after the range query
 * is done.
 *
 * Example: Query 5 entries starting from "hello".
 *     int i, ret;
 *     brt_it_t it;
 *     brt_qr_t ent;
 *     brt_lobnd(tree, &it, FASTR_LITERAL("hello"));
 *     for (i = 0; i < 5; i++) {
 *         ret = brt_it_next(&it, &ent, 0, 0);
 *         BUG_ON(ret < 0);
 *         process(&ent);
 *     }
 *     brt_it_close(&it);
 */
int brt_it_next(brt_it_t *it, brt_qr_t *ent, char delimiter);
void brt_it_close(brt_it_t *it);

/***************************************/
/*          Utility functions          */
/***************************************/

static inline int brt_is_empty(brt_tree_t *tree) {
    return !tree->root;
}

/*
 * These 3 functions are used for debugging and statistics. Ensure no
 * concurrent modification of @tree during invocation.
 */
void brt_verify(brt_tree_t *tree);
void brt_stat(brt_tree_t *tree, brt_stat_t *stat);
void brt_dump(brt_tree_t *tree);

/* Recovery */
void brt_recover_all(struct super_block *sb);

#endif //BRTREE_H
