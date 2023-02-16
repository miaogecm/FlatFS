/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 * 
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 * 
 * The Br tree implementation.
 */

#ifndef _BRTREE_H
#define _BRTREE_H

#include <linux/spinlock.h>
#include <linux/rwlock.h>
#include <linux/pathman.h>

#include "flatfs.h"
#include "brtree/ndcache.h"
#include "treeman.h"

/* The order of B+ Tree */
#define BRT_DEGREE          24
#define BRT_MAX_NR_CHLD       (BRT_DEGREE * 2)
#define BRT_MAX_NR_ENT        (BRT_DEGREE * 2 - 1)
#define BRT_MIN_NR_CHLD       (BRT_DEGREE)
#define BRT_MIN_NR_KEY        (BRT_DEGREE - 1)

/* No offset */
#define BRT_NOFF            0

/* The (padded) size of leaf/intn nodes */
#define BRT_LEAF_SIZE       1024
#define BRT_INTN_SIZE       1280

/* The initial node prefix buffer size */
#define BRT_NDPRE_INIT_SIZE 1024

/* safe padding bytes at ent/nodeblk header */
#define BRT_BLK_HDR_PADD    4

/* Size of node block and entry block */
#define BRT_NODE_BLK_SIZE   PAGE_SIZE
#define BRT_ENT_BLK_SIZE    PAGE_SIZE

/* Max number of nodes that a block contains */
#define BRT_LEAF_PER_BLK    (BRT_NODE_BLK_SIZE / BRT_LEAF_SIZE)
#define BRT_INTN_PER_BLK    (BRT_NODE_BLK_SIZE / BRT_INTN_SIZE)

/* Number of initially allocated blocks */
#define BRT_NR_NODE_BLOCK    8192  // 32 MB
#define BRT_N_ENT_BLOCK     65536 // 256 MB

/* Max prefetch size when prefetching from prefix to suffix */
#define BRT_PREFETCH_SIZE   32ul  // 32 B

/* Valid map */
#define BRT_VALIDMAP_SIZE   64
#define BRT_VALIDMAP_INIT   0ul
#define BRT_VALIDMAP_ENNR   (BRT_VALIDMAP_SIZE - 1)
#define BRT_VALIDMAP_ENMSK  (1ul << BRT_VALIDMAP_ENNR)
#define BRT_VALIDMAP_EMPTY  (BRT_VALIDMAP_INIT | BRT_VALIDMAP_ENMSK)
#define BRT_VALIDMAP_MAXID  (BRT_VALIDMAP_ENNR - 1)

/* Maximum tree height */
#define BRT_MAX_HEIGHT      8

/* Entry suffix buffer's block size, see @brt_ent_t */
#define BRT_SUF_BLK_SIZE    16

/*
 * FlatFS Br-Tree Entry
 *
 * Note that the @brt_ent_t itself only records the entry's metadata.
 * The real data (containing the pathname suffix, etc) resides outside
 * the struct, and can be referenced by @buf pointer. Note that the @buf
 * points to the **end**(HIGHMEM) of that buffer.
 *
 * The memory layout looks like:
 * --- LOWMEM
 * Inode number (8B, Only leaf nodes)
 * Perm suffix buffer (perm_suf_blkn * BRT_SUF_BLK_SIZE)
 * Path suffix buffer (path_suf_blkn * BRT_SUF_BLK_SIZE)
 * --- HIGHMEM
 * Path suffix buffer contains many *blocks*. Each
 * block has size @BRT_SUF_BLK_SIZE. The path suffix
 * buffer and perm suffix buffer contains @path_suf_blkn
 * and @perm_suf_blkn blocks respectively.
 *
 * The path suffix buffer can be further divided into 2 parts:
 * --- LOWMEM
 * Remaining cached path suffix (path_suf_cached - real_path_suf_len)
 * Real path suffix (path_len - node->path_pre_len)
 * --- HIGHMEM
 *
 * As well as the perm suffix buffer (Only leaf nodes):
 * --- LOWMEM
 * Remaining cached perm suffix (Multiple PPCs, total depth is
 *                               number of '/'s in remaining cached
 *                               path suffix)
 * Real perm suffix (Multiple PPCs, total depth is number of
 *                   '/'s in real path suffix)
 * --- HIGHMEM
 */
struct brt_ent_s {
    __le64 buf;
    __le16 path_len;
    __le16 path_suf_cached;
    __u8   path_suf_blkn;
    __u8   perm_suf_blkn;
    char   reserved[2];
} __packed;

/*
 * FlatFS BrTree node
 * Keep it aligned with XPLINE in NVM.
 *
 * Leaf node: 14cl (896B)
 * Internal node: 20cl (1280B)
 */
struct brt_node_s {
    /* Access the @brt_node_t in cacheline granularity */
    char cls[0][CACHELINE_SIZE];

    /* cacheline 0 */
    __u8   type;
    __u8   index;
    char   reserved[2];
    __le32 refcount;
    __le64 next;
    __le64 blocknr;
    /* TODO: dirtymap should be placed in cacheline 1! */
    __le64 dirtymap;
    __le64 prestorer;
    __le64 pre_buffer;
    char   padding0[8];

    rwlock_t *lock;

    /* cacheline 1 */
    __le64 validmap;
    __le16 path_pre_len;
    __le16 path_delta_len;
    __le16 perm_pre_len;
    __u8   slots[BRT_MAX_NR_ENT];
    char   padding1[3];

    /* cacheline [2, 13] */
    brt_ent_t entries[BRT_MAX_NR_ENT];
    char      padding3[16];

    /* cacheline [14, 19], only intn */
    __le64 children[];
} __packed;

/* A FlatFS BrTree in DRAM */
struct brt_tree_s {
    struct flatfs_sb_info *sbi;
    brt_ptree_t *ptree;
    /*
     * protects concurrent rw of root
     * field and ptree->root field.
     */
    rwlock_t root_lock;
    /* The calculated address of ptree->root */
	brt_node_t *root;
    /* Be calculated when mounting */
    size_t height;
    /* the node cache */
    struct ndcache *ndc;
};

/* the BrTree iterator, used for range query */
struct brt_it_s {
    brt_tree_t *tree;
    brt_node_t *start, *cur;
    fastr_t pathname;
    u8 cur_pos;
};

/* node block holds B+ tree leaf & internal node */
struct brt_nodeblk_hdr_s {
    /* leaf or internal */
    __le32      type;
    __le32		cpu;
    __le64		bitmap;
    /* block number */
    __le64 		blocknr;
    /* the block list */
    struct list_head list;
} __packed;

/* variable-sized entry block */
struct brt_entblk_hdr_s {
    /* how many entries */
    __le16 	num_entries;
    /* how much space inuse */
    __le16 	in_use;
    __le32  cpu;
    /* block number */
    __le64 	blocknr;
    /* which node owns this block */
    __le64	owner;
    spinlock_t *lock;
    /* the block list */
    struct list_head list;
    /* padding to a cl */
    char    padding[16];
} __packed;

/* statistics of a BrTree */
struct brt_stat_s {
    size_t height;
    size_t avg_suf_len;
};

/***************************************/
/*    BLOCK MANAGEMENT FUNCTIONS       */
/***************************************/

/* start/end of a ent/nodeblk page containing the specified addr */
#define BRT_BLK_START(addr)		\
    (((unsigned long) (addr)) & PAGE_MASK)
#define BRT_BLK_END(addr)			\
    (((((unsigned long) (addr)) >> PAGE_SHIFT) + 1) << PAGE_SHIFT)

/* Will the entblk be full after appending @len? */
static inline int brt_entblk_full(brt_entblk_hdr_t *hdr, size_t len) {
    return ((hdr->in_use + ALIGN(len, 8) > PAGE_SIZE) ||
            (hdr->in_use == PAGE_SIZE));
}

/* get nodeblk header of a nodeblk containing addr */
static inline brt_nodeblk_hdr_t *brt_nodeblk_hdr_of(void *addr) {
    unsigned long a = BRT_BLK_END(addr);
    a -= sizeof(brt_nodeblk_hdr_t) + BRT_BLK_HDR_PADD;
    return (brt_nodeblk_hdr_t *) a;
}

/* get entblk header of a entblk <sb, blocknr> */
static inline brt_entblk_hdr_t *brt_entblk_hdr_of(struct super_block* sb,
                                                  unsigned long blocknr) {
    u64 boff = flatfs_get_block_off(sb, blocknr, FLATFS_BLOCK_TYPE_4K);
    return (brt_entblk_hdr_t *) flatfs_get_block(sb, boff);
}

/***************************************/
/*      NODE MANAGEMENT FUNCTIONS      */
/***************************************/

static inline u8 brt_nd_nkeys(brt_node_t *node) {
    int shift = BRT_VALIDMAP_ENNR;
    return hweight64(node->validmap) - (node->validmap >> shift);
}

static inline brt_ndtype_t brt_nd_type(brt_node_t *node) {
    return node->type;
}

/* hold a reference of that node */
static inline void brt_nd_hold(brt_tree_t *tree, brt_node_t *node) {
    asm volatile(LOCK_PREFIX "incl %0" : "+m" (node->refcount));
}

/* release a reference of that node, return true if zero */
static inline int brt_nd_put_nofree(brt_tree_t *tree, brt_node_t *node) {
    GEN_UNARY_RMWcc(LOCK_PREFIX "decl", node->refcount, "%0", e);
}

/* release a reference of that node, free if zero */
static inline void brt_nd_put(brt_tree_t *tree, brt_node_t *node) {
    if (brt_nd_put_nofree(tree, node)) {
        brt_nd_free(tree, node);
    }
}

static inline brt_node_t *brt_nd_addr(brt_nodeblk_hdr_t *header,
                                      brt_ndtype_t type, size_t n) {
    unsigned long addr = BRT_BLK_START(header);
    if (type == BRT_LEAF) {
        addr += n * BRT_LEAF_SIZE;
    } else {
        addr += n * BRT_INTN_SIZE;
    }
    return (brt_node_t *) addr;
}

static inline brt_val_t *brt_nd_val_buf(brt_tree_t *tree, brt_node_t *node,
                                        brt_ent_t *ent, void *buffer) {
    size_t blk_off = ent->path_suf_blkn + ent->perm_suf_blkn;
    return (brt_val_t *) (buffer - blk_off * BRT_SUF_BLK_SIZE) - 1;
}

static inline brt_val_t *brt_nd_val(brt_tree_t *tree, brt_node_t *node,
                                    brt_ent_t *ent) {
    struct super_block *sb = tree->sbi->super;
    void *buffer = flatfs_offset_to_address(sb, ent->buf);
    return brt_nd_val_buf(tree, node, ent, buffer);
}

static inline brt_val_t brt_nd_val_n(brt_tree_t *tree, brt_node_t *node,
                                     u8 n) {
    brt_ent_t *ent = &node->entries[node->slots[n]];
    return *brt_nd_val(tree, node, ent);
}

static inline brt_node_t *brt_nd_chld_n(brt_tree_t *tree, brt_node_t *node,
                                        u8 n) {
    u8 i = (n > 0) ? (node->slots[n - 1] + 1) : 0;
    return (brt_node_t *) flatfs_offset_to_address(tree->sbi->super,
                                                   node->children[i]);
}

static inline brt_ent_t *brt_nd_ent_n(brt_tree_t *tree, brt_node_t *node,
                                      u8 n) {
    return &node->entries[node->slots[n]];
}

static inline void *brt_nd_entbuf(brt_tree_t *tree, brt_node_t *node,
                                  brt_ent_t *entry) {
    void *buf = flatfs_offset_to_address(tree->sbi->super, entry->buf);
    size_t off = 0;
    off += entry->path_suf_blkn * BRT_SUF_BLK_SIZE;
    if (brt_nd_type(node) == BRT_LEAF) {
        off += 8;
        off += entry->perm_suf_blkn * BRT_SUF_BLK_SIZE;
    }
    return buf - off;
}

static inline fastr_t brt_nd_path_pre(brt_tree_t *tree, brt_node_t *node) {
    struct super_block *sb = tree->sbi->super;
    void *buffer = flatfs_offset_to_address(sb, node->pre_buf);
    return fastr(buffer, le16_to_cpu(node->pre_len));
}

static inline size_t brt_nd_path_suf_len(brt_tree_t *tree, brt_node_t *node,
                                         brt_ent_t *ent) {
    u16 path_len = le16_to_cpu(ent->path_len) +
                   le16_to_cpu(node->delta_len);
    return path_len - le16_to_cpu(node->pre_len);
}

static inline size_t brt_nd_path_suf_len_n(brt_tree_t *tree,
                                           brt_node_t *node,
                                           u8 n) {
    return brt_nd_path_suf_len(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline fastr_t brt_nd_path_suf(brt_tree_t *tree, brt_node_t *node,
                                      brt_ent_t *ent) {
    struct super_block *sb = tree->sbi->super;
    size_t len = brt_nd_path_suf_len(tree, node, ent);
    void *buffer = flatfs_offset_to_address(sb, ent->buf);
    return fastr(buffer - len, len);
}

static inline fastr_t brt_nd_path_suf_n(brt_tree_t *tree, brt_node_t *node,
                                        u8 n) {
    return brt_nd_path_suf(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline fastr_t brt_nd_path_suf_cache(brt_tree_t *tree,
                                            brt_node_t *node,
                                            brt_ent_t *ent) {
    struct super_block *sb = tree->sbi->super;
    size_t len = le16_to_cpu(ent->path_suf_cached);
    void *buffer = flatfs_offset_to_address(sb, ent->buf);
    return fastr(buffer - len, len);
}

static inline fastr_t brt_nd_path_suf_cache_n(brt_tree_t *tree,
                                              brt_node_t *node,
                                              u8 n) {
    return brt_nd_path_suf_cache(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline ppcs_t brt_nd_perm_pre(brt_tree_t *tree, brt_node_t *node) {
    struct super_block *sb = tree->sbi->super;
    ppc_t *ppcarr = flatfs_offset_to_address(sb, node->pre_buf) +
                    BRT_NDPRE_INIT_SIZE;
    size_t nppc = PPCS_LEN2NPPC(le16_to_cpu(node->perm_pre_len));
    return ppcs_from_narr(ppcarr, nppc);
}

static inline size_t brt_nd_perm_suf_dep(brt_tree_t *tree, brt_node_t *node,
                                         brt_ent_t *ent) {
    fastr_t suf = brt_nd_path_suf(tree, node, ent);
    return get_path_depth(suf);
}

static inline size_t brt_nd_perm_suf_dep_n(brt_tree_t *tree, brt_node_t *node,
                                           u8 n) {
    return brt_nd_perm_suf_dep(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline size_t brt_nd_perm_suf_cache_dep(brt_tree_t *tree,
                                               brt_node_t *node,
                                               brt_ent_t *ent) {
    fastr_t suf = brt_nd_path_suf_cache(tree, node, ent);
    return get_path_depth(suf);
}

static inline size_t brt_nd_perm_suf_cache_dep_n(brt_tree_t *tree,
                                                 brt_node_t *node,
                                                 u8 n) {
    return brt_nd_perm_suf_cache_dep(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline ppcs_t brt_nd_perm_suf(brt_tree_t *tree, brt_node_t *node,
                                     brt_ent_t *ent, size_t *off) {
    struct super_block *sb = tree->sbi->super;
    size_t depth = brt_nd_perm_suf_dep(tree, node, ent);
    ppc_t *ppcarr = flatfs_offset_to_address(sb, ent->buf) -
                    ent->path_suf_blkn * BRT_SUF_BLK_SIZE;
    /* In fact, the real permstr for suf, which is consistent
     * with suf cache, has more levels than depth here, so let
     * exact = 0.
     */
    ppcs_t ps = ppcs_from_deparr_rv(ppcarr, depth, 0);
    *off = ppcs_depth(ps) - depth;
    return ps;
}

static inline ppcs_t brt_nd_perm_suf_n(brt_tree_t *tree, brt_node_t *node,
                                       u8 n, size_t *off) {
    return brt_nd_perm_suf(tree, node, brt_nd_ent_n(tree, node, n), off);
}

static inline ppcs_t brt_nd_perm_suf_cache(brt_tree_t *tree, brt_node_t *node,
                                            brt_ent_t *ent) {
    struct super_block *sb = tree->sbi->super;
    size_t depth = brt_nd_perm_suf_cache_dep(tree, node, ent);
    ppc_t *ppcarr = flatfs_offset_to_address(sb, ent->buf) -
                    ent->path_suf_blkn * BRT_SUF_BLK_SIZE;
    return ppcs_from_deparr_rv(ppcarr, depth, 1);
}

static inline ppcs_t brt_nd_perm_suf_cache_n(brt_tree_t *tree, brt_node_t *node,
                                             u8 n) {
    return brt_nd_perm_suf_cache(tree, node, brt_nd_ent_n(tree, node, n));
}

static inline void brt_nd_all_dirty(brt_tree_t *tree, brt_node_t *node) {
    node->dirtymap = U64_MAX;
}

static inline void brt_nd_set_dirty(brt_tree_t *tree, brt_node_t *node,
                                    u8 idx, int dirty) {
    u8 actual_idx = node->slots[idx];
    if (dirty) {
        __set_bit_le(actual_idx, &node->dirtymap);
    } else {
        __clear_bit_le(actual_idx, &node->dirtymap);
    }
}

/* @brt_nd_copy_dirty is atomic without concurrent modification of @node */
static inline void brt_nd_copy_dirty(brt_tree_t *tree,
                                     brt_node_t *dst_node, u8 dst_idx,
                                     brt_node_t *src_node, u8 src_idx) {
    int v = test_bit_le(src_node->slots[src_idx], &src_node->dirtymap);
    brt_nd_set_dirty(tree, dst_node, dst_idx, v);
}

static inline int brt_nd_is_dirty_n(brt_tree_t *tree, brt_node_t *node,
                                    u8 n) {
    return test_bit_le(node->slots[n], &node->dirtymap);
}

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
 *         ret = brt_it_next(&it, &ent);
 *         BUG_ON(ret < 0);
 *         process(&ent);
 *     }
 *     brt_it_close(&it);
 */
int brt_it_next(brt_it_t *it, brt_qr_t *ent);
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

#endif  /* _BRTREE_H */
