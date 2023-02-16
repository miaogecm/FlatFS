/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * The Br tree implementation.
 *
 * Lock order of BrTree:
 *      (1) Top->Down
 *      (2) Left->Right
 */

#include "brtree_old.h"
#include "brtree/bag.h"
#include "brtree/ndcache.h"
#include "treeman.h"
#include <linux/ppcs.h>
#include <linux/pathman.h>
#include <asm/byteorder.h>

typedef enum {
    DST_SRC_NO_RELA,
    DST_IS_SRC,
    SRC_IS_DST_CHLD,
    DST_IS_SRC_CHLD
} rela_hint_t;

#define BRT_VERIFY(x) \
	do { if (unlikely(!(x))) {       \
        printk("brtree_verifier: Oops!!! "   \
               "Malformed FlatFS Br Tree! node: %lx\n", \
               (unsigned long) node); \
        dump_node(tree, node);   \
        BUG();   \
	} } while (0)

static void dump_key(char *buf, brt_tree_t *tree,
                      brt_node_t *node, u8 n) {
    fastr_t str = fastr(buf, 0);
    fastr_append(&str, brt_nd_path_pre(tree, node));
    fastr_append(&str, brt_nd_path_suf_n(tree, node, n));
    buf[str.len] = '\0';
}

static void dump_node(brt_tree_t * tree, brt_node_t* node);

static int ppcs_well_compressed(ppcs_t ppcs) {
    size_t i, n_ppc = ppcs_n_ppc(ppcs);
    for (i = 0; i < n_ppc; i++) {
        ppc_t ppc = ppcs_get_ppc(ppcs, i);
        if (!ppc_get_dep(ppc)) {
            return 0;
        }
        if (i + 1 < n_ppc &&
            ppc_may_merge(ppcs_get_ppc(ppcs, i + 1), ppc)) {
            return 0;
        }
    }
    return 1;
}

static inline void verify_persistency(brt_tree_t *tree, brt_node_t *node, void *ptr) {
    if (brt_nd_type(node) == BRT_INTN) {
        BUG_ON(nvm_ptr(tree->sbi, ptr));
    } else {
        BUG_ON(!nvm_ptr(tree->sbi, ptr));
    }
}

static size_t _verify_node(brt_tree_t *tree, brt_node_t *node,
                           char *lo, char *hi,
                           brt_node_t **last_leaf_ptr) {
    struct super_block *sb = tree->sbi->super;

    unsigned long minw, maxw, min_suf, max_suf;
    brt_ndtype_t ndtype = brt_nd_type(node);
    size_t nkeys = brt_nd_nkeys(node);
    /* Do not use pathname longer than 256 chars in verify mode */
    char buf1[256], buf2[256];
    fastr_t mink, maxk;
    void *addr;
    size_t i;

    BRT_VERIFY(nkeys >= 1);

    /*
     * We validate the Br tree in the following aspects:
     * 1. The keys are sorted, i.e. k_0 < k_1 < ... < k_{n_k}.
     * 2. The keys are in valid range [lo, hi).
     * 3. Except the root node, the minimum key suffix and maximum
     *    key suffix must have different prefix.
     * 4. The permstr prefix and permstr suffix must be consistent
     *    with depth of path_pre and path_suf.
     * 5. If the node is root node:
     *      If it's also an internal node, 1 <= n_k <= 2d-1.
     *      If it's also an leaf node, 0 <= n_k <= 2d-1.
     *    Otherwise, d-1 <= n_k <= 2d-1.
     * 6. For internal node: n_c = n_k + 1, and c_0, c_1, ..., c_{n_c}
     *    are all valid.
     *    For leaf node: data_0, data_1, ..., data_{n_k} are all
     *    correct.
     * 7. All the leaf nodes are of equal height.
     * 8. The *next* fields of leaf nodes are all correct.
     */

    /* 1. */
    for (i = 1; i < nkeys; i++) {
        dump_key(buf1, tree, node, i);
        dump_key(buf2, tree, node, i - 1);
        BRT_VERIFY(strcmp(buf1, buf2) > 0);
    }

    /* 2. */
    dump_key(buf1, tree, node, 0);
    dump_key(buf2, tree, node, nkeys - 1);
    BRT_VERIFY(strcmp(buf1, lo) >= 0);
    BRT_VERIFY(strcmp(buf2, hi) < 0);

    /* 3. */
    mink = brt_nd_path_suf_n(tree, node, 0);
    maxk = brt_nd_path_suf_n(tree, node, nkeys - 1);
    minw = fastr_first_zpword(mink);
    maxw = fastr_first_zpword(maxk);
    min_suf = minw & 0xff;
    max_suf = maxw & 0xff;
    BRT_VERIFY((!min_suf && !max_suf) || (min_suf != max_suf));

    /* 4. */
    if (ndtype == BRT_LEAF) {
        BRT_VERIFY(get_path_depth(brt_nd_path_pre(tree, node)) ==
                   ppcs_depth(brt_nd_perm_pre(tree, node)));
        BRT_VERIFY(ppcs_well_compressed(brt_nd_perm_pre(tree, node)));
        for (i = 0; i < nkeys; i++) {
            size_t perm_suf_off;
            ppcs_t perm_suf = brt_nd_perm_suf_n(tree, node, i,
                                                &perm_suf_off);
            BRT_VERIFY(get_path_depth(brt_nd_path_suf_n(tree, node, i)) ==
                       ppcs_depth(perm_suf) - perm_suf_off);
            BRT_VERIFY(ppcs_well_compressed(perm_suf));
        }
    }

    /* 5. */
    if (node == tree->root) {
        if (ndtype == BRT_INTN) {
            BRT_VERIFY(nkeys >= 1);
            BRT_VERIFY(nkeys <= 2 * BRT_DEGREE - 1);
        } else if (ndtype == BRT_LEAF) {
            BRT_VERIFY(nkeys >= 0);
            BRT_VERIFY(nkeys <= 2 * BRT_DEGREE - 1);
        } else {
            BRT_VERIFY(0 && "Unknown node type!");
        }
    } else {
        BRT_VERIFY(nkeys >= BRT_DEGREE - 1);
        BRT_VERIFY(nkeys <= 2 * BRT_DEGREE - 1);
    }

    /* 6, 7 and 8. */
    if (ndtype == BRT_INTN) {
        size_t expected_height = -1, height;
        for (i = 0; i <= nkeys; i++) {
            char *lb = "\1", *rb = "\255";
            if (i > 0) {
                dump_key(buf1, tree, node, i - 1);
                lb = buf1;
            }
            if (i < nkeys) {
                dump_key(buf2, tree, node, i);
                rb = buf2;
            }
            addr = brt_nd_chld_n(tree, node, i);
            height = _verify_node(tree, addr, lb, rb, last_leaf_ptr);
            if (expected_height == -1) {
                expected_height = height;
            } else {
                BRT_VERIFY(height == expected_height);
            }
        }

        return expected_height + 1;
    } else if (node->type == BRT_LEAF) {
        if (*last_leaf_ptr) {
            addr = flatfs_offset_to_address(sb, (*last_leaf_ptr)->next);
            if (addr != node) {
                BRT_VERIFY(0 && "wrong last_leaf->next field!");
            }
        }
        *last_leaf_ptr = node;
        return 0;
    } else {
        BRT_VERIFY(0 && "Unknown node type!");
        unreachable();
    }
}

/* Make entries persistent selectively. (No fence) */
static void sel_persist_ents(brt_node_t *node, unsigned long victims) {
    /* batch flush, one cacheline at a time */
    brt_ent_t *ent = node->entries;
    for (; victims; victims >>= 4u, 4) {
        if (victims & 0xf) {
            flatfs_flush_buffer(ent, CACHELINE_SIZE, 0);
        }
    }
}

/* Make children persistent selectively. (No fence) */
static void sel_persist_children(brt_node_t *node, unsigned long victims) {
    /* batch flush, one cacheline at a time */
    __le64 *children = node->children;
    for (; victims; victims >>= 8u, children += 8) {
        if (victims & 0xff) {
            flatfs_flush_buffer(children, CACHELINE_SIZE, 0);
        }
    }
}

/*
 * Use a binary search to find the first ent whose key is
 * greater than or equal to @key. Return the ent's index.
 * If the ent's key is not strictly equal to @key, then
 * @is_not_equal will be set.
 */
static u8 keys_lowerbound(brt_tree_t *tree, brt_node_t *node,
                          brt_key_t key, int *is_not_equal) {
    fastr_t prefix = brt_nd_path_pre(tree, node);
    int neq = *is_not_equal = -1, cmp;
    u8 l = 0, r = brt_nd_nkeys(node);
    unsigned long mismatch;

    /*
     * In our BrTree's definition, the node can not be empty,
     * thus !r seems to be always false here. However, if a
     * node stays in a ndcache, then it may be empty and waiting
     * for being evicted.
     */
    if (unlikely(!r)) {
        return 0;
    }

    verify_persistency(tree, node, prefix.chars);

    /* Skip common prefix, then we only need to compare suffix. */
    fastr_eliminate(&key, &prefix);
    if ((mismatch = fastr_first_zpword(prefix))) {
        cmp = fastr_wordcmp(mismatch, fastr_first_zpword(key));
        if (cmp > 0) {
            return l;
        } else if (cmp < 0) {
            return r;
        }
    }

    while (l < r) {
        u8 mid = l + (r - l) / 2;
        fastr_t suf = brt_nd_path_suf_n(tree, node, mid);

        verify_persistency(tree, node, suf.chars);

        cmp = fastr_strcmp(suf, key);
        if (cmp < 0) {
            l = mid + 1;
        } else {
            r = mid;
        }

        /* use the 0th bit, no branch is needed here */
        neq &= cmp;
    }

    *is_not_equal = neq;
    return r;
}

static inline void node_kill(brt_tree_t *tree, brt_node_t *node) {
    BUG_ON(brt_nd_nkeys(node) != 0);
    brt_nd_put(tree, node);
}

/*
 * When we're moving an ent from @src to @dst, the ent's path_len
 * must be rebalanced based on dst_node's delta.
 *     src_delta + len = dst_delta + len' holds
 *                 len' = len + src_delta - dst_delta
 *
 * TODO: Crash consistency for @rebalance_delta
 */
static inline void rebalance_delta(brt_node_t *dst, brt_node_t *src,
                                   brt_ent_t *ent) {
    ent->path_len = cpu_to_le16(le16_to_cpu(ent->path_len) +
                                le16_to_cpu(src->delta_len) -
                                le16_to_cpu(dst->delta_len));
}

/*
 * Duplicate a leaf ent, from @src_ent to @dst_ent. The
 * @path_suf_blkn and @perm_suf_blkn will be both recalculated,
 * because when an ent's buffer is too small to apply a pre-
 * to-suf movement (which could happen after a rename op),
 * the function will be called to rebuild a suitable ent.
 *
 * Note that @dup_leaf_ent will not make @dst_ent persistent,
 * its the caller's responsibility. However, @dup_leaf_ent will
 * make the ent buffer persistent.
 *
 * Note that @dup_leaf_ent has a store fence.
 */
static void dup_leaf_ent(brt_tree_t *dst_tree,
                         brt_node_t *dst_node, brt_ent_t *dst_ent,
                         brt_tree_t *src_tree,
                         brt_node_t *src_node, brt_ent_t *src_ent) {
    struct super_block *sb = src_tree->sbi->super;

    size_t bufsz, blksz = BRT_SUF_BLK_SIZE;
    size_t path_len, perm_depth;
    size_t dst_path_suf_bufsz, dst_perm_suf_bufsz;
    size_t src_path_suf_bufsz, path_suf_cache_len, perm_suf_cache_len;
    void *dst_path_suf_cache, *dst_perm_suf_cache;
    void *src_path_suf_cache, *src_perm_suf_cache;
    void *dst_buf, *src_buf = flatfs_offset_to_address(sb, src_ent->buf);
    brt_val_t *dst_val, *src_val;

    /* recalculate path_suf_blkn and perm_suf_blkn */
    path_len = (u16) (le16_to_cpu(src_ent->path_len) +
                      le16_to_cpu(src_node->delta_len));
    perm_depth = ppcs_depth(brt_nd_perm_pre(src_tree, src_node));
    perm_depth += get_path_depth(brt_nd_path_suf(src_tree, src_node,
                                                 src_ent));
    dst_ent->path_suf_blkn = DIV_ROUND_UP(path_len, blksz);
    dst_ent->perm_suf_blkn = DIV_ROUND_UP(PPCS_SIZEOF(perm_depth), blksz);

    /* copy other fields */
    dst_ent->path_len = src_ent->path_len;
    dst_ent->path_suf_cached = src_ent->path_suf_cached;

    /* calculate total buffer size, and allocate memory for dst's entbuf */
    dst_path_suf_bufsz = dst_ent->path_suf_blkn * blksz;
    dst_perm_suf_bufsz = dst_ent->perm_suf_blkn * blksz;
    bufsz = dst_path_suf_bufsz + dst_perm_suf_bufsz + sizeof(brt_val_t);
    dst_buf = brt_entbuf_new(dst_tree, dst_node, bufsz) + bufsz;

    /* copy cached path, and cached perm from src_buf to dst_buf */
    src_path_suf_bufsz = src_ent->path_suf_blkn * blksz;
    path_suf_cache_len = dst_ent->path_suf_cached;
    perm_suf_cache_len = brt_nd_perm_suf_cache_dep(src_tree, src_node,
                                                   src_ent);
    perm_suf_cache_len = PPCS_SIZEOF(perm_suf_cache_len);
    dst_path_suf_cache = dst_buf - path_suf_cache_len;
    dst_perm_suf_cache = dst_buf - dst_path_suf_bufsz - perm_suf_cache_len;
    src_path_suf_cache = src_buf - path_suf_cache_len;
    src_perm_suf_cache = src_buf - src_path_suf_bufsz - perm_suf_cache_len;
    memcpy(dst_path_suf_cache, src_path_suf_cache, path_suf_cache_len);
    memcpy(dst_perm_suf_cache, src_perm_suf_cache, perm_suf_cache_len);

    /* copy value */
    dst_val = brt_nd_val_buf(dst_tree, dst_node, dst_ent, dst_buf);
    src_val = brt_nd_val(src_tree, src_node, src_ent);
    *dst_val = *src_val;

    /* make the ent buffer persistent, and fence */
    flatfs_flush_buffer(dst_path_suf_cache, path_suf_cache_len, 0);
    flatfs_flush_buffer(dst_perm_suf_cache, perm_suf_cache_len, 0);
    flatfs_flush_buffer(dst_val, sizeof(*dst_val), 1);

    /* activate the @dst_buf */
    dst_ent->buf = flatfs_address_to_offset(sb, dst_buf);
}

/*
 * Duplicate an internal ent, from @src_ent to @dst_ent. The
 * @path_suf_blkn will be recalculated, because when an ent's
 * buffer is too small to apply a p-to-suf movement (which
 * could happen after a rename op), the function will be called
 * to rebuild a suitable ent.
 *
 * Note that @dup_intn_ent will not make @dst_ent persistent,
 * its the caller's responsibility. However, @dup_intn_ent will
 * make the ent buffer persistent.
 *
 * Note that @dup_intn_ent has a store fence.
 *
 * Note that @src_node can be a leaf node. @dup_intn_ent will convert
 * the @src_ent to @dst_ent by removing value and PPCs.
 */
static void dup_intn_ent(brt_tree_t *dst_tree,
                         brt_node_t *dst_node, brt_ent_t *dst_ent,
                         brt_tree_t *src_tree,
                         brt_node_t *src_node, brt_ent_t *src_ent) {
    struct super_block *sb = src_tree->sbi->super;

    size_t bufsz, blksz = BRT_SUF_BLK_SIZE, path_len;
    size_t dst_path_suf_bufsz, path_suf_cache_len;
    void *dst_path_suf_cache, *src_path_suf_cache;
    void *dst_buf, *src_buf = flatfs_offset_to_address(sb, src_ent->buf);

    /* recalculate path_suf_blkn */
    path_len = (u16) (le16_to_cpu(src_ent->path_len) +
                      le16_to_cpu(src_node->delta_len));
    dst_ent->path_suf_blkn = DIV_ROUND_UP(path_len, blksz);

    /* copy other fields */
    dst_ent->path_len = src_ent->path_len;
    dst_ent->path_suf_cached = src_ent->path_suf_cached;

    /* calculate total buffer size, and allocate memory for dst's entbuf */
    dst_path_suf_bufsz = dst_ent->path_suf_blkn * blksz;
    bufsz = dst_path_suf_bufsz;
    dst_buf = brt_entbuf_new(dst_tree, dst_node, bufsz) + bufsz;

    /* copy cached path from src_buf to dst_buf */
    path_suf_cache_len = dst_ent->path_suf_cached;
    dst_path_suf_cache = dst_buf - path_suf_cache_len;
    src_path_suf_cache = src_buf - path_suf_cache_len;
    memcpy(dst_path_suf_cache, src_path_suf_cache, path_suf_cache_len);

    /* make the ent buffer persistent, and fence */
    flatfs_flush_buffer(dst_path_suf_cache, path_suf_cache_len, 1);

    /* activate the @dst_buf */
    dst_ent->buf = flatfs_address_to_offset(sb, dst_buf);
}

static void dup_node_pre(brt_tree_t *tree,
                         brt_node_t *dst, brt_node_t *src) {
    struct super_block *sb = tree->sbi->super;

    brt_ndtype_t dst_ty = brt_nd_type(dst);
    void *src_path_buf, *src_perm_buf;
    void *dst_path_buf, *dst_perm_buf;
    size_t off = BRT_NDPRE_INIT_SIZE;
    size_t path_len, perm_len;
    void *src_buf, *dst_buf;

    dst_buf = flatfs_offset_to_address(sb, dst->pre_buf);
    src_buf = flatfs_offset_to_address(sb, src->pre_buf);

    dst_path_buf = dst_buf;
    src_path_buf = src_buf;
    path_len = le16_to_cpu(src->pre_len);
    memcpy(dst_path_buf, src_path_buf, path_len);
    flatfs_flush_buffer(dst_path_buf, path_len, 0);
    dst->pre_len = src->pre_len;
    dst->delta_len = src->delta_len;

    if (dst_ty == BRT_LEAF) {
        dst_perm_buf = dst_buf + off;
        src_perm_buf = src_buf + off;
        perm_len = le16_to_cpu(src->perm_pre_len);
        memcpy(dst_perm_buf, src_perm_buf, perm_len);
        flatfs_flush_buffer(dst_perm_buf, perm_len, 0);
        dst->perm_pre_len = src->perm_pre_len;
    }

    /* It's the caller's responsibility to flush node metadata. */
}

/*
 * Rebuild an ent. When the ent buffer's size is not enough
 * (for example, after a rename operation), @rebuild_entry may be
 * called. @src specifies the current logical owner of @ent, and
 * @dst contains the new owner of @ent.
 *
 * The most confusing part is that why we need @src, @dst for
 * rebuilding, rather than only a single @node? Consider the
 * following case, when we're about to move an ent from a node to
 * another node. However, it fails due to lack of space. Now we have
 * to rebuild an ent, which belongs to @dst, but the ent's length
 * and other fields are calculated based on @src.
 *
 * Note that @rebuild_ent DO NOT MAKE @ent PERSISTENT. If you want to
 * guarantee that the write to @ent is atomic and durable, you should
 * pass an on-stack ent first, and use a 16B atomic operation to
 * override the node ent.
 */
static inline void rebuild_ent(brt_tree_t *tree,
                               brt_node_t *dst, brt_node_t *src,
                               brt_ent_t *ent) {
    struct super_block *sb = tree->sbi->super;
    void *old_buf = brt_nd_entbuf(tree, dst, ent);
    brt_ent_t new_ent;
    if (brt_nd_type(dst) == BRT_LEAF) {
        dup_leaf_ent(tree, dst, &new_ent, tree, src, ent);
    } else {
        dup_intn_ent(tree, dst, &new_ent, tree, src, ent);
    }
    *ent = new_ent;
    brt_entbuf_free(sb, old_buf);
}

static inline fastr_t expand_prefetch(fastr_t str, size_t max_len) {
    /* To ensure fetch at least BRT_PREFETCH_SIZE */
    size_t len = min(str.len, BRT_PREFETCH_SIZE);
    size_t n_prefetch = min(max_len, BRT_PREFETCH_SIZE - len);
    str.chars -= n_prefetch;
    str.len += n_prefetch;
    return str;
}

/*
 * Check if an ent is valid. If not, the path suffix cache, and
 * the perm suffix cache should be both invalidated (cleared).
 *
 * Consistency guarantee: @validate_ent_suf_cache only guarantees
 * that the original path suffix cache is consistent.
 */
static void validate_ent_suf_cache(brt_tree_t *tree,
                                   brt_node_t *node, u8 idx) {
    brt_ent_t *ent = brt_nd_ent_n(tree, node, idx);
    u16 path_suf_cached = brt_nd_path_suf_len(tree, node, ent);

    if (unlikely(brt_nd_is_dirty_n(tree, node, idx))) {
        /* Oops! Dirty suf cache, invalidate it. */
        if (brt_nd_type(node) == BRT_LEAF) {
            size_t off;
            ppcs_t perm_suf = brt_nd_perm_suf(tree, node, ent, &off);
            if (likely(perm_suf.len)) {
                ppc_t *addr = &perm_suf.words[0];
                ppc_set_dep(addr, ppc_get_dep(*addr) - off);
                flatfs_flush_buffer(addr, sizeof(*addr), 0);
            }
        }
        flatfs_memcpy_atomic(&ent->path_suf_cached, &path_suf_cached,
                             sizeof(path_suf_cached));
        flatfs_flush_buffer(&ent->path_suf_cached,
                            sizeof(ent->path_suf_cached), 0);

        /* revalidate the ent */
        brt_nd_set_dirty(tree, node, idx, 0);
        flatfs_flush_buffer(&node->dirtymap, sizeof(node->dirtymap), 0);
    }
}

int use_woc_key = 1;

/*
 * prepend @path_addend to ent suffix
 *
 * When moving ent from src to dst, and the ent needs to
 * be expanded, we should call ent_suf_prepend function. The
 * src is the *logical* owner of the ent, which means that
 * the ent can be either a child ent of src node itself,
 * or a copy of a src's ent.
 *
 * Note that the function only affects ent's path suffix,
 * permission suffix. Node's prefix, in-node dirty-map, or
 * delta will not be changed after invocation.
 *
 * You have to call @validate_ent_suf_cache explicitly to ensure that
 * @ent is not stale. The @ent may be not a child of src, so
 * we can not do it inside the function.
 *
 * If @ent is rebuilt due to lack of buffer size, the function
 * returns -ENOMEM, and the caller should handle the ent rep-
 * lacement. Otherwise, it returns 0.
 *
 * Consistency guarantee: @ent_suf_prepend only guarantees that
 * the original path suffix cache is consistent.
 */
static int ent_suf_prepend(brt_tree_t *tree,
                           brt_node_t *dst, brt_node_t *src,
                           brt_ent_t *ent,
                           fastr_t path_addend, size_t max_prefetch) {
    struct super_block *sb = tree->sbi->super;

    int ret = 0;
    void *new_path_suf_cache;
    size_t blksz = BRT_SUF_BLK_SIZE;

    size_t path_suf_len = brt_nd_path_suf_len(tree, src, ent);
    u16 required_suf_len, cached_suf_len, off;

    size_t old_dep, new_dep, perm_suf_dep, perm_dep;
    ppcs_t perm_cached;

    if (unlikely(!use_woc_key)) {
        char *prepend = brt_nd_path_suf(tree, src, ent).chars - path_addend.len;
        memcpy(prepend, path_addend.chars, path_addend.len);
        flatfs_flush_buffer(prepend, path_addend.len, 1);
    }

    /* Can we use the suffix cache directly? */
    required_suf_len = path_suf_len + path_addend.len;
    cached_suf_len = le16_to_cpu(ent->path_suf_cached);
    if (likely(required_suf_len <= cached_suf_len)) {
        goto out;
    }

    /* calculate prefetched pre-to-suf move size */
    off = required_suf_len - cached_suf_len;
    path_addend = expand_prefetch(fastr_slice_before(path_addend, off),
                                  max_prefetch);

    /* check if path suffix space is large enough */
    off = cached_suf_len + path_addend.len;
    if (unlikely(off > ent->path_suf_blkn * BRT_SUF_BLK_SIZE)) {
        rebuild_ent(tree, dst, src, ent);
        ret = -ENOMEM;
    }

    /* handle perm suf */
    if (brt_nd_type(dst) == BRT_LEAF) {
        /* We can not move from internal to leaf. */
        FLATFS_ASSERT(brt_nd_type(src) == BRT_LEAF);

        old_dep = get_path_depth(brt_nd_path_suf_cache(tree, src, ent));
        new_dep = get_path_depth(path_addend);

        perm_suf_dep = brt_nd_perm_suf_dep(tree, src, ent);
        perm_dep = perm_suf_dep + ppcs_depth(brt_nd_perm_pre(tree, src));

        /* check if perm suffix space is large enough */
        if (unlikely(perm_dep * sizeof(ppc_t) >
                     ent->perm_suf_blkn * blksz)) {
            rebuild_ent(tree, dst, src, ent);
            ret = -ENOMEM;
        }

        perm_cached = brt_nd_perm_suf_cache(tree, src, ent);
        ppcs_prepend(&perm_cached, brt_nd_perm_pre(tree, src),
                     old_dep - perm_suf_dep, new_dep);
        flatfs_flush_buffer(perm_cached.chars, perm_cached.len, 0);
    }

    /*
     * Prepend-only, the original path suf cache's
     * consistency is guaranteed.
     */
    new_path_suf_cache = flatfs_offset_to_address(sb, ent->buf) - off;
    memcpy(new_path_suf_cache, path_addend.chars, path_addend.len);
    flatfs_flush_buffer(new_path_suf_cache, path_addend.len, 0);

    ent->path_suf_cached = cpu_to_le16(cached_suf_len + path_addend.len);
    flatfs_flush_buffer(&ent->path_suf_cached,
                        sizeof(ent->path_suf_cached), 0);

out:
    return ret;
}

/*
 * append @path_addend to node prefix
 *
 * Note that the caller is responsible to flush @perm_pre_len
 * and @path_pre_len in @node.
 *
 * Consistency guarantee: @nd_pre_append only guarantees that
 * the original path prefix is consistent.
 */
static void nd_pre_append(brt_tree_t *tree,
                          brt_node_t *node, brt_ent_t *ent,
                          fastr_t path_addend) {
    void *path_buffer;

    size_t depth, perm_suf_off;
    ppcs_t perm_addend, perm_pre;

    path_buffer = flatfs_offset_to_address(tree->sbi->super,
                                           node->pre_buf);
    path_buffer += le16_to_cpu(node->pre_len);
    memcpy(path_buffer, path_addend.chars, path_addend.len);
    flatfs_flush_buffer(path_buffer, path_addend.len, 0);

    if (brt_nd_type(node) == BRT_LEAF) {
        depth = get_path_depth(path_addend);
        perm_addend = brt_nd_perm_suf(tree, node, ent, &perm_suf_off);
        perm_pre = brt_nd_perm_pre(tree, node);
        ppcs_append(&perm_pre, perm_addend, perm_suf_off, depth);
        flatfs_flush_buffer(perm_pre.chars, perm_pre.len, 0);

        node->perm_pre_len = cpu_to_le16(perm_pre.len);
    }

    node->pre_len = cpu_to_le16(le16_to_cpu(node->pre_len) +
                                path_addend.len);

    /* No flush @{path,perm}_pre_len here, caller's business. */
}

/*
 * shrink node prefix, remove last n characters from prefix
 *
 * Consistency guarantee: @nd_pre_shrink only guarantees that
 * the original path prefix is consistent.
 */
static inline void nd_pre_shrink(brt_tree_t *tree, brt_node_t *node,
                                 size_t n) {
    fastr_t path_pre = brt_nd_path_pre(tree, node);

    if (brt_nd_type(node) == BRT_LEAF) {
        ppcs_t perm_pre = brt_nd_perm_pre(tree, node);
        size_t perm_pre_shrink;

        perm_pre_shrink = get_path_depth(fastr_slice_lastn(path_pre, n));
        ppcs_shrink_suf(&perm_pre, perm_pre_shrink);
        flatfs_flush_buffer(perm_pre.chars, perm_pre.len, 0);
        node->perm_pre_len = cpu_to_le16(perm_pre.len);
    }

    node->pre_len = cpu_to_le16(path_pre.len - n);

    /* No flush @{path,perm}_pre_len here, caller's business. */
}

/* prepend @path_addend to all the entries of @node */
static void nd_suf_prepend(brt_tree_t *tree, brt_node_t *node,
                           fastr_t path_addend, size_t max_prefetch) {
    u8 n_keys = brt_nd_nkeys(node), i;
    unsigned long *_ent, *_tmp_ent;
    unsigned long victims = 0;
    brt_ent_t *ent, tmp_ent;
    int ret;

    if (unlikely(!path_addend.len)) {
        return;
    }

    for (i = 0; i < n_keys; i++) {
        validate_ent_suf_cache(tree, node, i);
        brt_nd_ent_n(tree, node, i);
        tmp_ent = *ent;
        ret = ent_suf_prepend(tree, node, node, &tmp_ent,
                              path_addend, max_prefetch);

        if (unlikely(ret == -ENOMEM)) {
            _ent = (unsigned long *) ent;
            _tmp_ent = (unsigned long *) &tmp_ent;
            /* make the rebuilding take effect atomically */
            ret = cmpxchg_double(&_ent[0], &_ent[1],
                                 _ent[0], _ent[1],
                                 _tmp_ent[0], _tmp_ent[1]);
            BUG_ON(!ret);
            /* just record, no flush now, to avoid cacheline thrashing */
            victims |= (1ul << node->slots[i]);
            continue;
        }
        BUG_ON(ret < 0);

        /* persist suf cache grow */
        if (ent->path_suf_cached != tmp_ent.path_suf_cached) {
            flatfs_memcpy_atomic(&ent->path_suf_cached,
                                 &tmp_ent.path_suf_cached,
                                 sizeof(tmp_ent.path_suf_cached));
            victims |= (1ul << node->slots[i]);
        }
    }

    sel_persist_ents(node, victims);
}

static inline void nd_suf_shrink(brt_tree_t *tree, brt_node_t *node,
                                 size_t n) {
    /*
     * Nothing has to be done.
     * Changes in the prefix length do the trick.
     */
}

/*
 * make node @a and @b have same path prefix
 *
 * Note that the caller is responsible to flush @perm_pre_len
 * and @path_pre_len in both @a and @b.
 *
 * Consistency guarantee: The function keeps the original
 * path pre of @a and @b, and all the entsuf paths of @a
 * and @b consistent.
 */
static void nd_pre2suf(brt_tree_t *tree,
                       brt_node_t *a, brt_node_t *b) {
    fastr_t pa = brt_nd_path_pre(tree, a);
    fastr_t pb = brt_nd_path_pre(tree, b);
    size_t pre_len = fastr_eliminate(&pa, &pb).len;

    nd_suf_prepend(tree, a, pa, pre_len);
    nd_suf_prepend(tree, b, pb, pre_len);

    nd_pre_shrink(tree, a, pa.len);
    nd_pre_shrink(tree, b, pb.len);
}

/*
 * expand common prefixes of entries to node prefix
 *
 * Consistency guarantee: The function keeps the original
 * path pre, and all the entsuf paths consistent.
 */
static void nd_suf2pre(brt_tree_t *tree, brt_node_t *node) {
    fastr_t min_key, max_key, prefix;
    u8 node_keyn = brt_nd_nkeys(node);

    if (unlikely(!node_keyn)) {
        return;
    }

    min_key = brt_nd_path_suf_n(tree, node, 0);
    max_key = brt_nd_path_suf_n(tree, node, node_keyn - 1);

    prefix = fastr_eliminate(&min_key, &max_key);

    nd_pre_append(tree, node, brt_nd_ent_n(tree, node, 0), prefix);
    nd_suf_shrink(tree, node, prefix.len);
}

/*
 * Add @ent to @dst at position @pos. The @ent originally
 * belongs to @src.
 *
 * The function will adjust @ent's suffix, @dst's prefix,
 * and other entries' suffix automatically.
 *
 * Note that it's the caller's responsibility to flush the
 * validmap, the slot array, the children array, the and
 * entries array.
 *
 * Consistency guarantee: The function keeps the original
 * path pre, and all the entsuf paths (of both @src and
 * @dst) consistent.
 */
static void nd_add_ent(/* insert to @dst at @pos */
                       brt_tree_t *tree, brt_node_t *dst, u8 pos,
                       /* from @ent in @src */
                       brt_node_t *src, brt_ent_t *ent, brt_node_t *child,
                       /*
                        * hint the relationship between @dst and @src
                        *
                        * If rela == SRC_IS_DST_CHLD, then @idx contains
                        * the index of @src in @dst. If rela == DST_IS_-
                        * SRC_CHLD, then @idx contains the index of @dst
                        * in @src. If rela == DST_IS_SRC, then @dst node
                        * is exactly @src node. Otherwise, @src and @dst
                        * have no relationship.
                        */
                       rela_hint_t rela, u8 idx) {
    struct super_block *sb = tree->sbi->super;

    fastr_t p = brt_nd_path_pre(tree, dst);
    fastr_t q = brt_nd_path_pre(tree, src);
    u8 dst_nkeys = brt_nd_nkeys(dst);
    u8 src_nkeys = brt_nd_nkeys(src);
    int tp, tq;

    __le64 *validmap;
    u8 new_idx;

    /*
     * When inserting into an empty node, the node's "prefix"
     * has not been well-defined. So we simply initialize the
     * prefix.
     */
    if (unlikely(dst_nkeys == 0)) {
        fastr_t path_suf = brt_nd_path_suf(tree, src, ent);
        fastr_append(&p, q);
        fastr_append(&p, path_suf);

        if (brt_nd_type(dst) == BRT_LEAF) {
            size_t off;
            ppcs_t ppcs_suf = brt_nd_perm_suf(tree, src, ent, &off);
            ppcs_t ppcs_pre = brt_nd_perm_pre(tree, src);
            ppcs_t ppcs_dst = brt_nd_perm_pre(tree, dst);
            ppcs_append(&ppcs_dst, ppcs_pre, 0, ppcs_depth(ppcs_pre));
            ppcs_append(&ppcs_dst, ppcs_suf,
                        off, brt_nd_perm_suf_dep(tree, src, ent));
            dst->perm_pre_len = cpu_to_le64(ppcs_dst.len);
        }

        dst->pre_len = cpu_to_le64(p.len);
    }

    /*
     * Adjust dst's prefix, dst entries' suffixes, and @ent's suffix via
     * @ent_suf_prepend, @nd_suf_prepend, @nd_pre_shrink.
     */

    if (rela == SRC_IS_DST_CHLD && (idx > 0 && idx < dst_nkeys)) {
        size_t n = p.len;
        fastr_shrink_pre(&p, n);
        fastr_shrink_pre(&q, n);
    } else if (rela == DST_IS_SRC_CHLD && (idx > 0 && idx < src_nkeys)) {
        size_t n = q.len;
        fastr_shrink_pre(&p, n);
        fastr_shrink_pre(&q, n);
    } else if (rela == DST_IS_SRC) {
        p = q = FASTR_NULL;
    } else {
        fastr_eliminate(&p, &q);
    }

    tp = !fastr_is_empty(p);
    tq = !fastr_is_empty(q);

    if (tq) {
        ent_suf_prepend(tree, dst, src, ent, q,
                        le16_to_cpu(src->pre_len) - q.len);
    } else if (tp) {
        fastr_t suf = brt_nd_path_suf(tree, src, ent);
        fastr_eliminate(&p, &suf);
        tp = !fastr_is_empty(p);
    }

    if (tp) {
        size_t path_pre_len = le16_to_cpu(dst->pre_len);
        nd_suf_prepend(tree, dst, p, path_pre_len - p.len);
        nd_pre_shrink(tree, dst, p.len);
    }

    /* balance delta */
    rebalance_delta(dst, src, ent);

    /* perform the actual insertion */
    validmap = &dst->validmap;
    new_idx = find_first_zero_bit_le(validmap, BRT_VALIDMAP_SIZE);
    BUG_ON(new_idx > BRT_VALIDMAP_MAXID);
    dst->entries[new_idx] = *ent;
    if (child) {
        /* this is for internal nodes */
        dst->children[new_idx + 1] = flatfs_address_to_offset(sb, child);
    }
    __set_bit_le(new_idx, validmap);

    /* update the slot array */
    memmove(&dst->slots[pos + 1],
            &dst->slots[pos], dst_nkeys - pos);
    dst->slots[pos] = new_idx;

    /* clear the dirty bit */
    brt_nd_set_dirty(tree, dst, pos, 0);
}

static void nd_del_ent(brt_tree_t *tree, brt_node_t *node, u8 pos) {
    u8 n_keys = brt_nd_nkeys(node);
    __clear_bit_le(node->slots[pos], &node->validmap);
    memmove(&node->slots[pos], &node->slots[pos + 1], n_keys - pos - 1);
    if (pos == 0 || pos == n_keys - 1) {
        /* Min/max key removed, node prefix may be expanded. */
        nd_suf2pre(tree, node);
    }
}

/*
 * Create a new leaf ent, and the new ent should be placed
 * at @idx later.
 *
 * Consistency guarantee: The function keeps the original path
 * pre, and all the entsuf paths consistent.
 */
static brt_ent_t new_leaf_ent(brt_tree_t *tree, brt_node_t *node, u8 idx,
                              brt_key_t key, brt_val_t val,
                              ppcs_t pppcs, ppc_t pppc) {
    struct super_block *sb = tree->sbi->super;

    size_t blksz = BRT_SUF_BLK_SIZE;
    unsigned long victims = 0;

    fastr_t pre = brt_nd_path_pre(tree, node);
    u8 n_keys = brt_nd_nkeys(node);
    size_t max_prefetch, depth;
    fastr_t path_addend;

    size_t pppc_dep = ppc_get_dep(pppc);
    size_t path_len = key.len;
    brt_ent_t ent;

    void *path_buf, *perm_buf;
    size_t ent_buf_size;
    ppcs_t perm_suf;

    brt_val_t *pval;

    /* skip prefix of @key, and put suf cache in @path_addend */
    if (idx == 0 || idx == n_keys) {
        size_t pre_len = fastr_eliminate(&pre, &key).len;
        ppcs_t perm_pre = brt_nd_perm_pre(tree, node);

        nd_suf_prepend(tree, node, pre, pre_len);
        node->pre_len = cpu_to_le16(pre_len);
        max_prefetch = pre_len;

        depth = get_path_depth(pre);
        ppcs_shrink_suf(&perm_pre, depth);
        node->perm_pre_len = cpu_to_le16(perm_pre.len);

        /* No flush @{path,perm}_pre_len here, caller's business. */
    } else {
        fastr_shrink_pre(&key, pre.len);
        max_prefetch = pre.len;
    }
    path_addend = expand_prefetch(key, max_prefetch);

    /* set ent meta */
    depth = ppcs_depth(pppcs) + pppc_dep;
    ent.path_suf_blkn = DIV_ROUND_UP(path_len, blksz);
    ent.perm_suf_blkn = DIV_ROUND_UP(PPCS_SIZEOF(depth), blksz);
    ent.path_suf_cached = cpu_to_le16(path_addend.len);
    path_len -= le16_to_cpu(node->delta_len);
    ent.path_len = cpu_to_le16(path_len);

    /* create ent buffer */
    ent_buf_size = 8 + (ent.path_suf_blkn + ent.perm_suf_blkn) * blksz;
    path_buf = brt_entbuf_new(tree, node, (int) ent_buf_size);
    path_buf += ent_buf_size;
    perm_buf = path_buf - ent.path_suf_blkn * blksz;
    ent.buf = flatfs_address_to_offset(sb, path_buf);

    /* fill-in the path ent buffer */
    memcpy(path_buf - path_addend.len,
           path_addend.chars, path_addend.len);
    flatfs_mark_region(path_buf - ent_buf_size, &victims,
                       path_buf - path_addend.len, path_addend.len);

    /* fill-in the perm ent buffer */
    depth = get_path_depth(path_addend);
    perm_suf = ppcs_from_narr(perm_buf, 0);
    if (depth >= pppc_dep) {
        ppcs_prepend_ppc(&perm_suf, pppc);
        depth = min(depth - pppc_dep, ppcs_depth(pppcs));
        ppcs_prepend(&perm_suf, pppcs, 0, depth);
        flatfs_mark_region(path_buf - ent_buf_size, &victims,
                           perm_suf.chars, perm_suf.len);
    }

    /* set value */
    pval = brt_nd_val(tree, node, &ent);
    *pval = val;
    flatfs_mark_region(path_buf - ent_buf_size, &victims,
                       pval, sizeof(*pval));

    flatfs_flush_regions(path_buf - ent_buf_size, &victims, 0);

    return ent;
}

static size_t get_height(brt_tree_t *tree) {
    struct super_block *sb = tree->sbi->super;
    brt_node_t *root = tree->root;
    size_t height = 0;
    if (unlikely(!root)) {
        goto out;
    }
    while (brt_nd_type(root) == BRT_INTN) {
        height++;
        root = flatfs_offset_to_address(sb, root->children[0]);
    }
    height++;
out:
    return height;
}

void brt_pin(brt_tree_t *dst, brt_ptree_t *src,
             struct flatfs_sb_info *sbi) {
    struct super_block *sb = sbi->super;

    /* only intact BrTrees have in-DRAM struct */
    BUG_ON(src->state != BRT_PT_INTACT);

    dst->root = src->root != BRT_NOFF
                    ? flatfs_offset_to_address(sb, src->root)
                    : NULL;
    dst->ptree = src;
    dst->sbi = sbi;
    dst->height = get_height(dst);
    dst->ndc = NULL;
    rwlock_init(&dst->crab_root_lock);

#ifdef CONFIG_FLATFS_DEBUG
    printk("Created In-DRAM BrTree %lx from In-NVM BrTree %lx."
           " root=%lx height=%lu\n",
           (ulong) dst, (ulong) src, (ulong) dst->root, dst->height);
#endif
}

extern brt_tree_t *main_brtree;

void brt_set_main(brt_tree_t *tree) {
    tree->ndc = kmalloc(sizeof(struct ndcache), GFP_ATOMIC);
    ndcache_init(tree);
    main_brtree = tree;
}

void brt_unset_main(brt_tree_t *tree) {
    ndcache_flush(tree);
    kfree(tree->ndc);
    tree->ndc = NULL;
}

/* split @parent's @idx th child atomically */
static brt_node_t *split(brt_tree_t *tree,
                         brt_node_t *parent, u8 idx,
                         u8 split_at) {
    struct super_block *sb = tree->sbi->super;

    DEFINE_TRANS_ON_STACK(trans);
    brt_node_t *node, *sibling;
    brt_ent_t *ent, _ent;
    u8 in, is, n, p;
    u8 start;

    node = brt_nd_chld_n(tree, parent, idx);

    /* create the sibling node */
    sibling = brt_nd_new(tree, brt_nd_type(node));
    dup_node_pre(tree, sibling, node);

    /* The split operation is a standalone transaction. */
    trans = flatfs_init_transaction(sb, trans, 4);

    /*
     * Temporarily disable validmap, to ensure that the enable
     * bit of the recovered validmap is 0.
     */
    __clear_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);

    flatfs_add_logentry(sb, trans, &parent->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &node->cls[1], CACHELINE_SIZE, LE_DATA);

    __set_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);

    validate_ent_suf_cache(tree, node, split_at);
    brt_nd_ent_n(tree, node, split_at);
    if (brt_nd_type(node) == BRT_LEAF) {
        dup_intn_ent(tree, parent, &_ent, tree, node, ent);
        start = split_at;
    } else {
        start = split_at + 1;
    }
    nd_add_ent(tree, parent, idx,
               node, ent, sibling,
               SRC_IS_DST_CHLD, idx);
    __clear_bit_le(node->slots[split_at], &node->validmap);

    n = brt_nd_nkeys(node);
    for (in = start, is = 0; in <= n; in++, is++) {
        p = node->slots[in];
        sibling->slots[is] = is;
        sibling->entries[is] = node->entries[p];
        __clear_bit_le(p, &node->validmap);
        __set_bit_le(is, &sibling->validmap);
        brt_nd_copy_dirty(tree, sibling, is, node, in);
    }

    if (brt_nd_type(node) == BRT_INTN) {
        sibling->children[0] = node->children[node->slots[split_at] + 1];
        for (in = start, is = 0; in <= n; in++, is++) {
            p = node->slots[in];
            sibling->children[is + 1] = node->children[p + 1];
        }
    }

    /* sibling and node only have half keys, try to expand prefix */
    nd_suf2pre(tree, sibling);
    nd_suf2pre(tree, node);

    sibling->next = node->next;
    node->next = flatfs_address_to_offset(sb, sibling);

    flatfs_flush_buffer(&sibling->cls[0], 3 * CACHELINE_SIZE, 0);
    flatfs_flush_buffer(&sibling->entries, is * sizeof(brt_ent_t), 0);
    flatfs_flush_buffer(&sibling->children, is * sizeof(__le64), 0);
    flatfs_flush_buffer(&parent->cls[1], CACHELINE_SIZE, 0);
    flatfs_flush_buffer(&node->next, sizeof(node->next), 0);
    flatfs_flush_buffer(&node->cls[1], CACHELINE_SIZE, 1);

    flatfs_commit_transaction(sb, trans);

    return sibling;
}

/*
 * Pull one entry from the right sibling node atomically.
 *
 * dst: @parent's @idx th child
 * src: @parent's @idx+1 th child
 */
static void share_r(brt_tree_t *tree, brt_node_t *parent, u8 idx) {
    struct super_block *sb = tree->sbi->super;

    DEFINE_TRANS_ON_STACK(trans);
    brt_node_t *dst, *src, *ch;
    brt_ent_t *ent, _ent;
    u8 pi, pei, dn;
    int nclines;

    dst = brt_nd_chld_n(tree, parent, idx);
    src = brt_nd_chld_n(tree, parent, idx + 1);

    dn = brt_nd_nkeys(dst);

    pi = idx;
    pei = parent->slots[pi];

    /* The share operation is a standalone transaction. */
    nclines = 8 + (brt_nd_type(dst) == BRT_INTN ? 1 : 0);
    trans = flatfs_init_transaction(sb, trans, nclines);

    /*
     * Temporarily disable validmap, to ensure that the enable
     * bit of the recovered validmap is 0.
     */
    __clear_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &src->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &dst->validmap);

    flatfs_add_logentry(sb, trans, &parent->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &src->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &dst->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &parent->entries[pei], sizeof(brt_ent_t),
                        LE_DATA);
    flatfs_add_logentry(sb, trans, &parent->children[pei + 1], sizeof(__le64),
                        LE_DATA);
    if (brt_nd_type(src) == BRT_INTN) {
        flatfs_add_logentry(sb, trans, &src->children[0], sizeof(__le64),
                            LE_DATA);
    }

    __set_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &src->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &dst->validmap);

    if (brt_nd_type(dst) == BRT_INTN) {
        validate_ent_suf_cache(tree, parent, pi);
        brt_nd_ent_n(tree, parent, pi);
        nd_add_ent(tree, dst, dn,
                   parent, ent, brt_nd_chld_n(tree, src, 0),
                   DST_IS_SRC_CHLD, idx);
        nd_del_ent(tree, parent, pi);

        validate_ent_suf_cache(tree, src, 0);
        brt_nd_ent_n(tree, src, 0);
        nd_add_ent(tree, parent, pi,
                   src, ent, src,
                   DST_SRC_NO_RELA, -1);
        ch = brt_nd_chld_n(tree, src, 1);
        src->children[0] = flatfs_address_to_offset(sb, ch);
        nd_del_ent(tree, src, 0);
    } else {
        validate_ent_suf_cache(tree, src, 0);
        brt_nd_ent_n(tree, src, 0);
        nd_add_ent(tree, dst, dn,
                   src, ent, NULL,
                   DST_SRC_NO_RELA, 0);
        nd_del_ent(tree, parent, pi);

        validate_ent_suf_cache(tree, src, 1);
        brt_nd_ent_n(tree, src, 1);
        dup_intn_ent(tree, parent, &_ent, tree, src, ent);
        nd_add_ent(tree, parent, pi,
                   src, ent, src,
                   DST_SRC_NO_RELA, -1);
        nd_del_ent(tree, src, 0);
    }

    if (brt_nd_type(src) == BRT_INTN) {
        flatfs_flush_buffer(&src->children[0], sizeof(__le64), 0);
    }
    flatfs_flush_buffer(&src->cls[1], CACHELINE_SIZE, 0);
    flatfs_flush_buffer(&parent->entries[pei], sizeof(brt_ent_t), 0);
    flatfs_flush_buffer(&parent->cls[1], CACHELINE_SIZE, 0);
    flatfs_flush_buffer(&dst->entries[dst->slots[dn]], sizeof(brt_ent_t), 0);
    flatfs_flush_buffer(&dst->cls[1], CACHELINE_SIZE, 1);

    flatfs_commit_transaction(sb, trans);
}

/*
 * Pull one entry from the left sibling node atomically.
 *
 * dst: @parent's @idx th child
 * src: @parent's @idx-1 th child
 */
static void share_l(brt_tree_t *tree, brt_node_t *parent, u8 idx) {
    struct super_block *sb = tree->sbi->super;

    DEFINE_TRANS_ON_STACK(trans);
    brt_node_t *dst, *src, *ch;
    brt_ent_t *ent, _ent;
    u8 pi, pei, sn;
    int nclines;

    dst = brt_nd_chld_n(tree, parent, idx);
    src = brt_nd_chld_n(tree, parent, idx - 1);

    sn = brt_nd_nkeys(src);

    pi = idx - 1;
    pei = parent->slots[pi];

    /* The share operation is a standalone transaction. */
    nclines = 8 + (brt_nd_type(dst) == BRT_INTN ? 1 : 0);
    trans = flatfs_init_transaction(sb, trans, nclines);

    /*
     * Temporarily disable validmap, to ensure that the enable
     * bit of the recovered validmap is 0.
     */
    __clear_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &src->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &dst->validmap);

    flatfs_add_logentry(sb, trans, &parent->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &src->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &dst->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &parent->entries[pei], sizeof(brt_ent_t),
                        LE_DATA);
    flatfs_add_logentry(sb, trans, &parent->children[pei + 1], sizeof(__le64),
                        LE_DATA);
    if (brt_nd_type(dst) == BRT_INTN) {
        flatfs_add_logentry(sb, trans, &dst->children[0], sizeof(__le64),
                            LE_DATA);
    }

    __set_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &src->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &dst->validmap);

    if (brt_nd_type(dst) == BRT_INTN) {
        validate_ent_suf_cache(tree, parent, pi);
        brt_nd_ent_n(tree, parent, pi);
        nd_add_ent(tree, dst, 0,
                   parent, ent, brt_nd_chld_n(tree, dst, 0),
                   DST_IS_SRC_CHLD, idx);
        ch = brt_nd_chld_n(tree, src, sn);
        dst->children[0] = flatfs_address_to_offset(sb, ch);
        nd_del_ent(tree, parent, pi);

        validate_ent_suf_cache(tree, src, sn - 1);
        brt_nd_ent_n(tree, src, sn - 1);
        nd_add_ent(tree, parent, pi,
                   src, ent, dst,
                   DST_SRC_NO_RELA, -1);
        nd_del_ent(tree, src, sn - 1);
    } else {
        validate_ent_suf_cache(tree, src, sn - 1);
        brt_nd_ent_n(tree, src, sn - 1);
        nd_add_ent(tree, dst, 0,
                   src, ent, NULL,
                   DST_SRC_NO_RELA, 0);
        nd_del_ent(tree, parent, pi);

        dup_intn_ent(tree, parent, &_ent, tree, dst, ent);
        nd_add_ent(tree, parent, pi,
                   dst, ent, dst,
                   DST_SRC_NO_RELA, -1);
        nd_del_ent(tree, src, sn - 1);
    }

    if (brt_nd_type(dst) == BRT_INTN) {
        flatfs_flush_buffer(&dst->children[0], sizeof(__le64), 0);
    }
    flatfs_flush_buffer(&dst->entries[dst->slots[0]], sizeof(brt_ent_t), 0);
    flatfs_flush_buffer(&dst->cls[1], CACHELINE_SIZE, 1);
    flatfs_flush_buffer(&src->cls[1], CACHELINE_SIZE, 0);
    flatfs_flush_buffer(&parent->entries[pei], sizeof(brt_ent_t), 0);
    flatfs_flush_buffer(&parent->cls[1], CACHELINE_SIZE, 0);

    flatfs_commit_transaction(sb, trans);
}

/* Fuse parent's idx th child and idx+1 th child. */
static brt_node_t *fuse(brt_tree_t *tree, brt_node_t **pparent, u8 idx) {
    struct super_block *sb = tree->sbi->super;

    brt_node_t *l, *r, *chld, *parent = *pparent;
    unsigned long ent_victims, chld_victims;
    int free_parent = 0, free_entbuf = 0;
    DEFINE_TRANS_ON_STACK(trans);
    u8 lnk, rnk, i, valid_idx;
    brt_ndtype_t ty;
    brt_ent_t pent;

    l = brt_nd_chld_n(tree, parent, idx);
    r = brt_nd_chld_n(tree, parent, idx + 1);
    ty = brt_nd_type(l);
    lnk = brt_nd_nkeys(l);
    rnk = brt_nd_nkeys(r);

    trans = flatfs_init_transaction(sb, trans, 7);

    /*
     * Temporarily disable validmap, to ensure that the enable
     * bit of the recovered validmap is 0.
     */
    __clear_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &l->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &r->validmap);

    flatfs_add_logentry(sb, trans, &parent->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &l->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &r->cls[1], CACHELINE_SIZE, LE_DATA);

    __set_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &l->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &r->validmap);

    /* Get the connector key. */
    validate_ent_suf_cache(tree, parent, idx);
    pent = *brt_nd_ent_n(tree, parent, idx);
    nd_del_ent(tree, parent, idx);
    flatfs_flush_buffer(&parent->cls[1], CACHELINE_SIZE, 0);
    if (brt_nd_nkeys(parent) == 0) {
        flatfs_add_logentry(sb, trans,
                            tree->ptree, sizeof(brt_ptree_t), LE_DATA);
        tree->ptree->root = flatfs_address_to_offset(sb, l);
        eflatfs_flush_buffer(tree->ptree, sizeof(brt_ptree_t), 0);
        tree->root = l;
        tree->height--;
        write_unlock(parent->lock);
        free_parent = 1;
    }

    ent_victims = 0;
    chld_victims = 0;

    /* The connector key is only useful for internal nodes. */
    if (ty == BRT_LEAF) {
        free_entbuf = 1;
    } else {
        chld = ty == BRT_INTN ? brt_nd_chld_n(tree, r, 0) : NULL;
        nd_add_ent(tree,
                   l, lnk,
                   parent, &pent, chld,
                   DST_IS_SRC_CHLD, idx);
        ent_victims |= 1ul << l->slots[lnk];
        chld_victims |= 1ul << (l->slots[lnk] + 1);
        lnk++;
    }

    /* Merge with the whole right node. */
    nd_pre2suf(tree, l, r);
    for (i = 0; i < rnk; i++, lnk++) {
        valid_idx = find_first_zero_bit_le(&l->validmap, BRT_VALIDMAP_SIZE);
        l->slots[lnk] = valid_idx;
        l->entries[valid_idx] = *brt_nd_ent_n(tree, r, i);
        ent_victims |= 1ul << valid_idx;
        if (ty == BRT_INTN) {
            l->children[valid_idx + 1] = r->children[r->slots[i] + 1];
            chld_victims |= 1ul << (valid_idx + 1);
        }
        __set_bit_le(valid_idx, &l->validmap);
        brt_nd_copy_dirty(tree, l, lnk, r, i);
        rebalance_delta(l, r, &l->entries[valid_idx]);
    }
    /* TODO: @next pointer's crash consistency */
    l->next = r->next;

    flatfs_flush_buffer(&l->cls[1], CACHELINE_SIZE, 0);
    sel_persist_ents(l, ent_victims);
    if (ty == BRT_INTN) {
        sel_persist_children(l, chld_victims);
    }

    flatfs_commit_transaction(sb, trans);

    if (free_entbuf) {
        brt_entbuf_free(sb, brt_nd_entbuf(tree, parent, &pent));
    }
    if (free_parent) {
        write_unlock(parent->lock);
        node_kill(tree, parent);
        *pparent = NULL;
    }
    r->validmap = BRT_VALIDMAP_EMPTY;
    write_unlock(r->lock);
    node_kill(tree, r);

    return l;
}

static inline int cmp_with_presuf(fastr_t pre, fastr_t suf, fastr_t str) {
    unsigned long mismatch;
    int cmp;
    fastr_eliminate(&pre, &str);
    if ((mismatch = fastr_first_zpword(pre)) &&
        (cmp = fastr_wordcmp(mismatch, fastr_first_zpword(str)))) {
        return cmp;
    }
    return fastr_strcmp(suf, str);
}

static inline void tree_walk_cache(brt_tree_t *tree, brt_node_t *node) {
    if (likely(tree->ndc)) {
        ndcache_insert(tree, node);
    }
}

int ndcache_enable = 1;

static int tree_walk_fastpath(brt_tree_t *tree, brt_key_t key,
                              brt_node_t **dnode, u8 *dpos, int *dne,
                              int wlock, u8 lbnd, u8 ubnd) {
    int is_not_equal = -1, found_node = 0, ret = -ENOENT;
    struct ndcache_entry *entry;
    brt_node_t *node;
    u8 pos, nkeys;

    if (unlikely(!ndcache_enable)) {
        goto out;
    }

    if (unlikely(!tree->ndc)) {
        goto out;
    }

    preempt_disable();

    ndcache_for_each_entry(tree->ndc, entry, {
        if (wlock) {
            write_lock(entry->node->lock);
        } else {
            read_lock(entry->node->lock);
        }
        pos = keys_lowerbound(tree, entry->node, key, &is_not_equal);
        ndcache_compare();
        nkeys = brt_nd_nkeys(entry->node);
        if (!is_not_equal ||
            (pos > 0 && (pos < nkeys || entry->node->next == BRT_NOFF))) {
            found_node = 1;
            /*
             * The entry->node may be changed when read lock is released.
             * We should save it in a local variable.
             */
            node = entry->node;
            break;
        }
        if (wlock) {
            write_unlock(entry->node->lock);
        } else {
            read_unlock(entry->node->lock);
        }
    });

    if (found_node) {
        if (nkeys >= lbnd && nkeys <= ubnd) {
            *dnode = node;
            *dpos = pos;
            *dne = is_not_equal;
            ndcache_access(tree, entry, node);
            ret = 0;
        } else {
            if (wlock) {
                write_unlock(node->lock);
            } else {
                read_unlock(node->lock);
            }
        }
    }

    preempt_enable();

out:
    return ret;
}

static long __brt_add(brt_tree_t *tree,
                      brt_key_t k, brt_val_t v,
                      ppcs_t pppcs, ppc_t pppc, bool update) {
    struct super_block *sb = tree->sbi->super;

    brt_node_t *node, *parent, *new_root;
    DEFINE_TRANS_ON_STACK(trans);
    int is_not_equal, cmp;
    long ret;
    u8 pos;

    /* the leaf insertion transaction */
    trans = flatfs_init_transaction(sb, trans, 2);

    /* Try fast-path first. */
    if (!tree_walk_fastpath(tree, k, &node, &pos, &is_not_equal,
                            1, 0, BRT_MAX_NR_ENT - 1)) {
        goto do_add;
    }

    parent = NULL;
    pos = 0;

    /* Increase the tree height if needed. */
    new_root = NULL;
    write_lock(&tree->crab_root_lock);
    node = tree->root;
    if (unlikely(!node)) {
        new_root = node = brt_nd_new(tree, BRT_LEAF);
    }
    write_lock(node->lock);
    if (unlikely(brt_nd_nkeys(node) == BRT_MAX_NR_ENT)) {
        /* To split the root node, we have to create a new root. */
        new_root = parent = brt_nd_new(tree, BRT_INTN);
        parent->children[0] = flatfs_address_to_offset(sb, node);
        write_lock(parent->lock);
    }
    if (unlikely(new_root)) {
        __le64 noff = flatfs_address_to_offset(sb, new_root);
        tree->root = new_root;
        tree->height++;
        flatfs_memcpy_atomic(&tree->ptree->root, &noff, sizeof(noff));
        flatfs_flush_buffer(&tree->ptree->root, sizeof(noff), 1);
    }
    write_unlock(&tree->crab_root_lock);

    /*
     * Find which node we insert the key into, while splitting preemptively
     * along the path.
     */
    while (1) {
        u8 nkeys = brt_nd_nkeys(node);

        /* overflow check */
        if (nkeys == BRT_MAX_NR_ENT) {
            brt_node_t *sibling;
            size_t cut = nkeys / 2;
            FLATFS_ASSERT(parent);
            cmp = cmp_with_presuf(brt_nd_path_pre(tree, node),
                                  brt_nd_path_suf_n(tree, node, cut), k);
            sibling = split(tree, parent, pos, cut);
            if (cmp <= 0) {
                /*
                 * If split_key <= key, then must be in the right sibling of
                 * node. We move like a crab here.
                 */
                write_lock(sibling->lock);
                write_unlock(node->lock);
                node = sibling;
            }
        }

        if (parent) {
            write_unlock(parent->lock);
        }

        pos = keys_lowerbound(tree, node, k, &is_not_equal);

        if (node->type == BRT_LEAF) {
            break;
        }

        parent = node;
        if (!is_not_equal) {
            pos++;
        }
        node = brt_nd_chld_n(tree, node, pos);
        write_lock(node->lock);
    }

    tree_walk_cache(tree, node);

do_add:
    FLATFS_ASSERT(node->type == BRT_LEAF);

    if (!update) {
        /* leaf insert */
        if (likely(is_not_equal)) {
            brt_ent_t ent;

            __clear_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);
            flatfs_add_logentry(sb, trans, &node->cls[1],
                                CACHELINE_SIZE, LE_DATA);
            __set_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);

            ent = new_leaf_ent(tree, node, pos, k, v, pppcs, pppc);
            nd_add_ent(tree, node, pos, node, &ent, NULL, DST_IS_SRC, 0);
            flatfs_flush_buffer(&node->entries[node->slots[pos]],
                                sizeof(brt_ent_t), 0);

            /* flush metadata cacheline */
            flatfs_flush_buffer(&node->cls[1], CACHELINE_SIZE, 1);

            flatfs_commit_transaction(sb, trans);

            ret = 0;
        } else {
            flatfs_abort_transaction(sb, trans);

            ret = (long) brt_nd_val_n(tree, node, pos);
        }
    } else {
        /* leaf update */
        if (likely(!is_not_equal)) {
            brt_val_t *valp = brt_nd_val(tree, node, brt_nd_ent_n(tree, node, pos));

            *valp = v;

            flatfs_commit_transaction(sb, trans);

            ret = 0;
        } else {
            flatfs_abort_transaction(sb, trans);

            ret = -ENOENT;
        }
    }
    write_unlock(node->lock);

    return ret;
}

long brt_add(brt_tree_t *tree,
             brt_key_t k, brt_val_t v,
             ppcs_t pppcs, ppc_t pppc) {
#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "add", tree);
    brbag_fastr(&lb, k);
    brbag_value(&lb, v);
    brbag_ppcs(&lb, pppcs);
    brbag_ppc(&lb, pppc);
    brbag_end(&lb);
#endif

    FLATFS_INFO(BRTREE_ADD, "(%lu) %.*s$ [%lu] -> %lu",
                k.len, FASTR_FMT(k), ppcs_depth(pppcs) + ppc_get_dep(pppc), v);

    return __brt_add(tree, k, v, pppcs, pppc, false);
}

int brt_upd(brt_tree_t *tree, brt_key_t k, brt_val_t v) {
#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "upd", tree);
    brbag_fastr(&lb, k);
    brbag_value(&lb, v);
    brbag_end(&lb);
#endif

    FLATFS_INFO(BRTREE_UPD, "(%lu) %.*s$ -> %lu", k.len, FASTR_FMT(k), v);

    return (int) __brt_add(tree, k, v, PPCS_NULL, 0, true);
}

int brt_del(brt_tree_t* tree, brt_key_t key) {
    struct super_block *sb = tree->sbi->super;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

    DEFINE_TRANS_ON_STACK(trans);
    brt_node_t *parent, *node;
    brt_node_t *lsib, *rsib;
    int root_unlocked;
    int is_not_equal;
    int ret;
    u8 pos;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "del", tree);
    brbag_fastr(&lb, key);
    brbag_end(&lb);
#endif

    FLATFS_INFO(BRTREE_DEL, "(%lu) %.*s$", key.len, FASTR_FMT(key));

    /* the leaf deletion transaction */
    trans = flatfs_init_transaction(sb, trans, 2);

    /* Try fast-path first. */
    if (!tree_walk_fastpath(tree, key, &node, &pos, &is_not_equal,
                            1, BRT_MIN_NR_KEY + 1, -1)) {
        root_unlocked = 1;
        goto do_del;
    }

    root_unlocked = 0;
    write_lock(&tree->crab_root_lock);
    node = tree->root;
    if (unlikely(!node)) {
        ret = -ENOENT;
        write_unlock(&tree->crab_root_lock);
        goto out;
    }
    write_lock(node->lock);

    /* We do not have to do underflow check for root node. */
    goto start;

    while (1) {
        u8 nkeys = brt_nd_nkeys(node);

        /* underflow check */
        if (nkeys == BRT_MIN_NR_KEY) {
            brt_node_t *sib = NULL;

            if (rsib) {
                write_lock(rsib->lock);
            }

            FLATFS_ASSERT(lsib || rsib);
            if (!lsib) {
                sib = rsib;
            } else if (!rsib) {
                sib = lsib;
            } else {
                sib = brt_nd_nkeys(rsib) > brt_nd_nkeys(lsib) ? rsib : lsib;
            }

            if (brt_nd_nkeys(sib) != BRT_MIN_NR_KEY) {
                ((sib == lsib) ? share_l : share_r)(tree, parent, pos);
            } else {
                if (sib == lsib) {
                    node = fuse(tree, &parent, pos - 1);
                    lsib = NULL;
                } else {
                    node = fuse(tree, &parent, pos);
                    rsib = NULL;
                }
            }

            if (rsib) {
                write_unlock(rsib->lock);
            }
        }

        if (lsib) {
            write_unlock(lsib->lock);
        }

        if (!root_unlocked) {
            root_unlocked = 1;
            write_unlock(&tree->crab_root_lock);
        }

        if (parent) {
            write_unlock(parent->lock);
        }

start:
        pos = keys_lowerbound(tree, node, key, &is_not_equal);

        if (brt_nd_type(node) == BRT_LEAF) {
            break;
        }

        parent = node;
        if (!is_not_equal) {
            pos++;
        }
        node = brt_nd_chld_n(tree, parent, pos);
        lsib = pos == 0
                ? NULL
                : brt_nd_chld_n(tree, parent, pos - 1);
        rsib = pos == brt_nd_nkeys(parent)
                ? NULL
                : brt_nd_chld_n(tree, parent, pos + 1);

        if (lsib) {
            write_lock(lsib->lock);
        }
        write_lock(node->lock);
    }

    tree_walk_cache(tree, node);

do_del:
    FLATFS_ASSERT(node->type == BRT_LEAF);

    /* Decrease height if necessary. */
    if (unlikely(brt_nd_nkeys(node) == 1 && !is_not_equal)) {
        __le64 noff = BRT_NOFF;
        FLATFS_ASSERT(tree->root == node);
        tree->root = NULL;
        tree->height--;
        flatfs_memcpy_atomic(&tree->ptree->root, &noff, sizeof(noff));
        flatfs_flush_buffer(&tree->ptree->root, sizeof(noff), 0);
    }

    if (!root_unlocked) {
        write_unlock(&tree->crab_root_lock);
    }

    /* leaf remove */
    if (likely(!is_not_equal)) {
        brt_ent_t *ent = &node->entries[node->slots[pos]];

        __clear_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);
        flatfs_add_logentry(sb, trans, &node->cls[1],
                            CACHELINE_SIZE, LE_DATA);
        __set_bit_le(BRT_VALIDMAP_ENNR, &node->validmap);

        nd_del_ent(tree, node, pos);

        /* flush metadata cacheline */
        flatfs_flush_buffer(&node->cls[1], CACHELINE_SIZE, 1);

        flatfs_commit_transaction(sb, trans);

        brt_entbuf_free(sb, brt_nd_entbuf(tree, node, ent));

        ret = 0;
    } else {
        flatfs_abort_transaction(sb, trans);

        ret = -ENOENT;
    }
    write_unlock(node->lock);

    if (unlikely(!brt_nd_nkeys(node))) {
        node_kill(tree, node);
    }

out:
    return ret;
}

void brt_dup(brt_tree_t *dst, brt_tree_t *src) {
    /* TODO: Change to DFS version */
    struct super_block *sb = src->sbi->super;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

    struct fifo {
        brt_node_t *node;
        struct fifo *next;

        brt_node_t *parent;
        u8 chld;
    };

    struct fifo *fifo = kmalloc(sizeof(*fifo), GFP_KERNEL);
    struct fifo *tail = fifo, *tmp;
    brt_node_t *last_node = NULL;
    u8 isol;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "dup", dst);
    brbag_tree(&lb, src);
    brbag_end(&lb);
#endif

    /* @dst should be an empty BrTree. */
    FLATFS_ASSERT(dst->ptree->state == BRT_PT_INTACT);
    FLATFS_ASSERT(!dst->ptree->root);

    /* auto free when crashed */
    isol = BRT_PT_ISOLAT;
    flatfs_memcpy_atomic(&dst->ptree->state, &isol, sizeof(__u8));
    flatfs_flush_buffer(&dst->ptree->state, sizeof(__u8), 1);

    /* copy in-DRAM tree */
    dst->sbi = src->sbi;
    dst->height = src->height;
    rwlock_init(&dst->crab_root_lock);

    fifo->node = src->root;
    fifo->parent = NULL;
    fifo->next = NULL;

    while (fifo) {
        brt_node_t *dup_node = brt_nd_new(dst, brt_nd_type(fifo->node));
        u8 nkeys = brt_nd_nkeys(fifo->node), i;
        typeof(dup_intn_ent) *duper;
        unsigned long entmap = 0;
        brt_ent_t _ent;

        /* copy prefix */
        dup_node_pre(dst, dup_node, fifo->node);

        /* copy node metadata */
        dup_node->dirtymap = fifo->node->dirtymap;
        memcpy(&dup_node->cls[1], &fifo->node->cls[1], CACHELINE_SIZE);
        flatfs_flush_buffer(&dup_node->cls[1], CACHELINE_SIZE, 0);

        /* copy node entries */
        /* TODO: Free entries' memory after crash. */
        duper = (brt_nd_type(fifo->node) == BRT_LEAF)
                    ? dup_leaf_ent
                    : dup_intn_ent;
        for (i = 0; i < nkeys; i++) {
            duper(dst, dup_node, &_ent,
                  src, fifo->node, brt_nd_ent_n(src, fifo->node, i));
            dup_node->entries[dup_node->slots[i]] = _ent;
            /* just record, no flush now, to avoid cacheline thrashing */
            entmap |= (1ul << dup_node->slots[i]);
        }
        sel_persist_ents(dup_node, entmap);

        /* copy child, and @next pointer */
        if (fifo->parent) {
            u8 cid = fifo->chld
                        ? fifo->parent->slots[fifo->chld - 1] + 1
                        : 0;
            fifo->parent->children[cid] = flatfs_address_to_offset(sb,
                                                                   dup_node);

            /*
             * NOTE: To avoid branching, we set *next* for both internal or leaf
             *       node. In fact, only leaf node's *next* is meaningful.
             */
            FLATFS_ASSERT(last_node);
            last_node->next = flatfs_address_to_offset(sb, dup_node);
        } else {
            dst->root = dup_node;
        }

        if (brt_nd_type(fifo->node) == BRT_INTN) {
            for (i = 0; i <= nkeys; i++) {
                struct fifo *cur = kmalloc(sizeof(*cur), GFP_KERNEL);
                cur->node = brt_nd_chld_n(dst, fifo->node, i);
                cur->next = NULL;
                cur->parent = dup_node;
                cur->chld = i;
                tail = tail->next = cur;
            }
        }

        last_node = dup_node;
        tmp = fifo;
        fifo = fifo->next;
        kfree(tmp);
    }
}

/* NOTE: The caller is responsible to unlock the leaf node. */
static brt_node_t *lowerbound(brt_tree_t *tree, brt_key_t key,
                              u8 *index, int *is_not_equal) {
    int is_not_equal_;
    brt_node_t *node;
    u8 pos;

    /* Try fast-path first. */
    if (!tree_walk_fastpath(tree, key, &node, &pos, &is_not_equal_,
                            0, 0, -1)) {
        goto do_lowerbound;
    }

    read_lock(&tree->crab_root_lock);
    node = tree->root;
    if (!node) {
        read_unlock(&tree->crab_root_lock);
        goto out;
    }
    read_lock(node->lock);
    read_unlock(&tree->crab_root_lock);

    while (1) {
        brt_node_t *child;

        pos = keys_lowerbound(tree, node, key, &is_not_equal_);
        if (brt_nd_type(node) == BRT_INTN) {
            if (!is_not_equal_) {
                pos++;
            }
            child = brt_nd_chld_n(tree, node, pos);
            read_lock(child->lock);
            read_unlock(node->lock);
            node = child;
        } else {
            break;
        }
    }

    tree_walk_cache(tree, node);

do_lowerbound:
    *index = pos;
    *is_not_equal = is_not_equal_;
out:
    return node;
}

int brt_get(brt_tree_t *tree, brt_qr_t *res, brt_key_t key) {
    struct super_block *sb = tree->sbi->super;

    fastr_t path_pre, path_suf;
    ppcs_t perm_pre, perm_suf;
    int ret = 0, is_not_equal;
    brt_node_t *node;
    void *buf;
    u8 pos;

    /*
     * Valid modes:
     * RAW + PERM
     * SEM + PERM
     * RAW
     * SEM
     */
    if ((res->mode & 3) != 1 && (res->mode & 3) != 2) {
        ret = -EINVAL;
        goto out;
    }

    FLATFS_INFO(BRTREE_GET, "(%lu) %.*s$", key.len, FASTR_FMT(key));

    res->path = FASTR_NULL;
    res->ppcs = PPCS_NULL;

    node = lowerbound(tree, key, &pos, &is_not_equal);

    if (unlikely(!node)) {
        ret = -ENOENT;
        goto out_unlock;
    }

    if (unlikely(pos == brt_nd_nkeys(node))) {
        /* goto next leaf node */
        brt_node_t *next;
        if (unlikely(node->next == BRT_NOFF)) {
            ret = -ENOENT;
            goto out_unlock;
        }
        next = flatfs_offset_to_address(sb, node->next);
        read_lock(next->lock);
        read_unlock(node->lock);
        node = next;
        pos = 0;
    }

    path_pre = brt_nd_path_pre(tree, node);
    path_suf = brt_nd_path_suf_n(tree, node, pos);

    if (res->mode & BRT_QR_SEM) {
        if (unlikely(is_not_equal)) {
            fastr_t last, _key = key;
            size_t skip = 0;

            skip += fastr_eliminate(&path_pre, &_key).len;
            last = path_pre;
            if (fastr_is_empty(path_pre)) {
                skip += fastr_eliminate(&path_suf, &_key).len;
                last = path_suf;
            }

            if (likely(path_pre.len + path_suf.len == 1 &&
                       last.chars[0] == CTLCHR_PREFIX_INTERCEPTOR)) {
                if (!fastr_is_empty(_key)) {
                    ret = -EAGAIN;
                }
                res->path = fastr_slice_before(key, skip - 1);
            } else {
                ret = -ENOENT;
                goto out_unlock;
            }
        } else {
            res->path = key;
        }
    } else if (res->mode & BRT_QR_RAW) {
        if (unlikely(is_not_equal)) {
            ret = -ENOENT;
            goto out_unlock;
        }
    }

    if (res->mode & BRT_QR_PERM) {
        size_t perm_off, perm_sz;
        perm_pre = brt_nd_perm_pre(tree, node);
        perm_suf = brt_nd_perm_suf_n(tree, node, pos, &perm_off);
        perm_sz = perm_pre.len + perm_suf.len;
        if (likely(perm_sz <= sizeof(res->embed_ppcs))) {
            buf = res->embed_ppcs;
        } else {
            buf = kmalloc(perm_sz, GFP_ATOMIC);
            if (unlikely(!buf)) {
                /* TODO: free path memory */
                ret = -ENOMEM;
                goto out_unlock;
            }
        }
        res->ppcs = ppcs_from_narr(buf, 0);
        ppcs_append(&res->ppcs, perm_pre, 0, ppcs_depth(perm_pre));
        ppcs_append(&res->ppcs, perm_suf,
                    perm_off, ppcs_depth(perm_suf) - perm_off);
    }

    res->inode = brt_nd_val_n(tree, node, pos);

out_unlock:
    read_unlock(node->lock);
out:
    return ret;
}

void brt_got(brt_tree_t *tree, brt_qr_t *res) {
    if (res->ppcs.words != res->embed_ppcs) {
        kfree(res->ppcs.chars);
    }
}

static brt_node_t *tree_min(brt_tree_t *tree,
                            brt_node_t *root) {
    while (brt_nd_type(root) != BRT_LEAF) {
        root = brt_nd_chld_n(tree, root, 0);
    }
    return root;
}

static brt_node_t *tree_max(brt_tree_t *tree,
                            brt_node_t *root) {
    while (brt_nd_type(root) != BRT_LEAF) {
        root = brt_nd_chld_n(tree, root, brt_nd_nkeys(root));
    }
    return root;
}

static int cmp_nd_keys(brt_tree_t *tree,
                       brt_node_t *node1, u8 idx1,
                       brt_node_t *node2, u8 idx2) {
    fastr_t suf1 = brt_nd_path_suf_n(tree, node1, idx1);
    fastr_t suf2 = brt_nd_path_suf_n(tree, node2, idx2);
    fastr_t pre1 = brt_nd_path_pre(tree, node1);
    fastr_t pre2 = brt_nd_path_pre(tree, node2);
    char *buf1, *buf2;
    fastr_t fs1, fs2;
    int cmp;

    /* TODO: Get rid of those annoying kmalloc and memcpy */
    buf1 = kmalloc(pre1.len + suf1.len, GFP_ATOMIC);
    buf2 = kmalloc(pre2.len + suf2.len, GFP_ATOMIC);
    fs1 = fastr(buf1, 0);
    fs2 = fastr(buf2, 0);

    fastr_append(&fs1, pre1);
    fastr_append(&fs1, suf1);
    fastr_append(&fs2, pre2);
    fastr_append(&fs2, suf2);

    cmp = fastr_strcmp(fs1, fs2);

    kfree(buf1);
    kfree(buf2);

    return cmp;
}

static int do_absorb(brt_tree_t *dst, brt_tree_t *src) {
    struct super_block *sb = dst->sbi->super;

    int cmp, free_right, par_add_ent, par_relink_chld, nc, ret = 0;
    u8 i, j, pos, dkn, skn, tkn, lkn, rkn, div, valid_idx;
    brt_node_t *src_min, *src_max, *dst_min, *dst_max;
    brt_node_t *dn, *sn, *ln, *rn, *parent, *child;
    u8 *pslot, tmp_slots[BRT_MAX_NR_ENT];
    DEFINE_TRANS_ON_STACK(trans);
    unsigned long victims;
    brt_tree_t *lt, *rt;
    struct {
        brt_node_t *owner;
        brt_ent_t ent;
        __le64 lc, rc;
        u8 idx;
    } *entries, *pent;
    brt_ndtype_t ty;
    size_t hdiff;
    __le64 vmp;

    /* @src and @dst must be of the same FlatFS namespace. */
    FLATFS_ASSERT(dst->sbi == src->sbi);

    /* Tall tree absorb short tree. */
    if (dst->height < src->height) {
        swap(*dst, *src);
    }

    /* Handle special case: @src is empty. */
    if (brt_is_empty(src)) {
        goto out;
    }

    src_min = tree_min(src, src->root);
    dst_min = tree_min(dst, dst->root);
    src_max = tree_max(src, src->root);
    dst_max = tree_max(dst, dst->root);

    /* cmp: dst - src */
    if (cmp_nd_keys(dst, dst_min, 0, src_max, 0) > 0) {
        /* src dst */
        cmp = 1;
    } else if (cmp_nd_keys(dst, src_min, 0, dst_max, 0) > 0) {
        /* dst src */
        cmp = -1;
    } else {
        BUG();
    }

    pos = 0;
    parent = NULL;
    dn = dst->root;
    hdiff = dst->height - src->height;

    /* Increase the tree height if needed. */
    if (unlikely(brt_nd_nkeys(dn) == BRT_MAX_NR_ENT)) {
        __le64 noff;
        /* To split the root node, we have to create a new root. */
        parent = brt_nd_new(dst, BRT_INTN);
        parent->children[0] = flatfs_address_to_offset(sb, dn);
        dst->root = parent;
        dst->height++;
        noff = flatfs_address_to_offset(sb, parent);
        flatfs_memcpy_atomic(&dst->ptree->root, &noff, sizeof(noff));
        flatfs_flush_buffer(&dst->ptree->root, sizeof(noff), 1);
    }

    /* walk to the same height (bottom-up height) */
    while (hdiff--) {
        brt_node_t *leftmost = dn, *rightmost = dn;
        u8 nkeys = brt_nd_nkeys(dn);
        FLATFS_ASSERT(brt_nd_type(dn) == BRT_INTN);
        /* overflow check */
        if (nkeys == BRT_MAX_NR_ENT) {
            FLATFS_ASSERT(parent);
            rightmost = split(dst, parent, pos, nkeys / 2);
        }
        /* walk down one level, through the left/rightmost path */
        if (cmp < 0) {
            parent = rightmost;
            pos = brt_nd_nkeys(rightmost);
        } else {
            parent = leftmost;
            /* keep pos at 0 */
        }
        dn = brt_nd_chld_n(dst, parent, pos);
    }
    sn = src->root;
    dkn = brt_nd_nkeys(dn);
    skn = brt_nd_nkeys(sn);
    ty = brt_nd_type(dn);
    FLATFS_ASSERT(ty == brt_nd_type(sn));

    tkn = dkn + skn;
    if (ty == BRT_INTN) {
        /* Need a "connector" entry for internal nodes. */
        tkn++;
    }

    if (cmp < 0) {
        ln = dn;
        rn = sn;
        lt = dst;
        rt = src;
        lkn = dkn;
        rkn = skn;
    } else {
        ln = sn;
        rn = dn;
        lt = src;
        rt = dst;
        lkn = skn;
        rkn = dkn;
    }

    pent = entries = kmalloc(sizeof(*entries) * tkn, GFP_ATOMIC);
    if (!pent) {
        ret = -ENOMEM;
        goto out;
    }

    /* Too much keys leads to split, which may change @parent. */
    par_add_ent = 0;
    if (tkn > BRT_MAX_NR_ENT) {
        par_add_ent = 1;
        /* Increase the tree height if needed. */
        if (unlikely(!parent)) {
            __le64 noff;
            parent = brt_nd_new(dst, BRT_INTN);
            parent->children[0] = flatfs_address_to_offset(sb, dn);
            dst->root = parent;
            dst->height++;
            noff = flatfs_address_to_offset(sb, parent);
            flatfs_memcpy_atomic(&dst->ptree->root, &noff, sizeof(noff));
            flatfs_flush_buffer(&dst->ptree->root, sizeof(noff), 1);
        }
    }

    /*
     * Change
     *       parent
     *       /(c0)
     * sn   dn
     * to
     *       parent
     *      /
     *     sn+dn
     */
    par_relink_chld = 0;
    if (!par_add_ent && cmp > 0) {
        BUG_ON(ln != sn);
        if (parent) {
            par_relink_chld = 1;
        } else {
            dst->root = src->root;
        }
    }

    nc = 5;
    nc += par_add_ent ? 2 : 0;
    nc += par_relink_chld ? 1 : 0;

    /* start the transaction */
    trans = flatfs_init_transaction(sb, trans, nc);

    /*
     * Temporarily disable validmap, to ensure that the enable
     * bit of the recovered validmap is 0.
     */
    if (par_add_ent) {
        __clear_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    }
    __clear_bit_le(BRT_VALIDMAP_ENNR, &dn->validmap);
    __clear_bit_le(BRT_VALIDMAP_ENNR, &sn->validmap);

    flatfs_add_logentry(sb, trans,
                        &src->ptree->root, sizeof(src->ptree->root), LE_DATA);
    flatfs_add_logentry(sb, trans, &dn->cls[1], CACHELINE_SIZE, LE_DATA);
    flatfs_add_logentry(sb, trans, &sn->cls[1], CACHELINE_SIZE, LE_DATA);
    if (par_add_ent) {
        flatfs_add_logentry(sb, trans, &parent->cls[1], CACHELINE_SIZE, LE_DATA);
    }
    if (par_relink_chld) {
        flatfs_add_logentry(sb, trans, &parent->children[0],
                            sizeof(parent->children[0]), LE_DATA);
    }

    if (par_add_ent) {
        __set_bit_le(BRT_VALIDMAP_ENNR, &parent->validmap);
    }
    __set_bit_le(BRT_VALIDMAP_ENNR, &dn->validmap);
    __set_bit_le(BRT_VALIDMAP_ENNR, &sn->validmap);

    if (par_relink_chld) {
        parent->children[0] = flatfs_address_to_offset(sb, ln);
    }

    /* Update the @next field */
    /* TODO: Crash consistency of @next */
    if (ty == BRT_INTN || par_add_ent) {
        child = tree_min(rt, rn);
        tree_max(lt, ln)->next = flatfs_address_to_offset(sb, child);
    } else {
        /* Leaf node, and they should be fused into @ln. */
        ln->next = rn->next;
    }

    nd_pre2suf(dst, dn, sn);

    /* Aggregate */
    for (i = 0; i < lkn; i++) {
        pent->ent = *brt_nd_ent_n(lt, ln, i);
        pent->owner = ln;
        pent->idx = i;
        if (ty == BRT_INTN) {
            child = brt_nd_chld_n(lt, ln, i);
            pent->lc = flatfs_address_to_offset(sb, child);
            child = brt_nd_chld_n(lt, ln, i + 1);
            pent->rc = flatfs_address_to_offset(sb, child);
        }
        pent++;
    }
    if (ty == BRT_INTN) {
        /* Let's deal with the connector entry. */
        fastr_t pre, conn_pre;
        brt_tree_t *conn_tr;
        brt_node_t *conn_nd;
        brt_ent_t *conn_ent;
        u8 conn_idx = 0;

        if (cmp < 0) {
            /* dst conn src */
            conn_tr = src;
            conn_nd = src_min;
            child = brt_nd_chld_n(dst, dn, dkn);
            pent->lc = flatfs_address_to_offset(sb, child);
            child = brt_nd_chld_n(src, sn, 0);
            pent->rc = flatfs_address_to_offset(sb, child);
        } else {
            /* src conn dst */
            conn_tr = dst;
            conn_nd = dst_min;
            child = brt_nd_chld_n(src, sn, skn);
            pent->lc = flatfs_address_to_offset(sb, child);
            child = brt_nd_chld_n(dst, dn, 0);
            pent->rc = flatfs_address_to_offset(sb, child);
        }

        validate_ent_suf_cache(dst, conn_nd, conn_idx);
        conn_ent = brt_nd_ent_n(conn_tr, conn_nd, conn_idx);
        dup_intn_ent(dst, dn, &pent->ent, conn_tr, conn_nd, conn_ent);

        /*
         * Now we have to adjust the connector entry to fit the @dn's
         * path prefix. Note that the @conn_nd's path prefix is the
         * prefix of @dn's prefix.
         */
        pre = brt_nd_path_pre(dst, dn);
        conn_pre = brt_nd_path_pre(conn_tr, conn_nd);
        fastr_shrink_pre(&conn_pre, pre.len);
        ent_suf_prepend(dst, dn, conn_nd, &pent->ent, conn_pre, pre.len);

        pent->owner = conn_nd;
        pent->idx = conn_idx;
        pent++;
    }
    for (i = 0; i < rkn; i++) {
        pent->ent = *brt_nd_ent_n(rt, rn, i);
        pent->owner = rn;
        pent->idx = i;
        if (ty == BRT_INTN) {
            child = brt_nd_chld_n(rt, rn, i);
            pent->lc = flatfs_address_to_offset(sb, child);
            child = brt_nd_chld_n(rt, rn, i + 1);
            pent->rc = flatfs_address_to_offset(sb, child);
        }
        pent++;
    }

    /* Distribute */
    div = tkn;
    if (div > BRT_MAX_NR_ENT) {
        div /= 2;
    }

    i = 0;

    /* < div: left node */
    vmp = BRT_VALIDMAP_EMPTY;
    victims = 0;
    for (; i < div; i++) {
        if (entries[i].owner == ln) {
            valid_idx = ln->slots[entries[i].idx];
        } else {
            valid_idx = find_first_zero_bit_le(&vmp, BRT_VALIDMAP_SIZE);
            ln->slots[lkn] = valid_idx;
            ln->entries[valid_idx] = entries[i].ent;
            if (ty == BRT_INTN) {
                ln->children[valid_idx + 1] = entries[i].rc;
            }
            victims |= 1ul << valid_idx;
            __set_bit_le(valid_idx, &vmp);
            brt_nd_copy_dirty(dst, ln, lkn, entries[i].owner, entries[i].idx);
            rebalance_delta(ln, entries[i].owner, &ln->entries[valid_idx]);
            lkn++;
        }
        __set_bit_le(valid_idx, &vmp);
    }
    ln->validmap = vmp;
    nd_suf2pre(dst, ln);
    /* persistent the left node */
    flatfs_flush_buffer(&ln->cls[1], CACHELINE_SIZE, 0);
    sel_persist_ents(ln, victims);
    if (ty == BRT_INTN) {
        sel_persist_children(ln, victims << 1);
    }

    /* = div: hoist */
    if (i < tkn) {
        brt_ent_t *tmp, _tmp;
        validate_ent_suf_cache(dst, entries[i].owner, entries[i].idx);
        tmp = brt_nd_ent_n(dst, entries[i].owner, entries[i].idx);
        entries[i].ent.path_suf_cached = tmp->path_suf_cached;
        if (ty == BRT_INTN) {
            tmp = &entries[i].ent;
        } else {
            dup_intn_ent(dst, parent, &_tmp,
                         dst, entries[i].owner, tmp);
            tmp = &_tmp;
        }
        if (cmp < 0) {
            /* dst src */
            nd_add_ent(dst,
                       parent, pos,
                       entries[i].owner, tmp, sn,
                       DST_SRC_NO_RELA, -1);
        } else {
            /* src dst */
            child = brt_nd_chld_n(dst, parent, 0);
            nd_add_ent(dst,
                       parent, pos,
                       entries[i].owner, tmp, child,
                       DST_SRC_NO_RELA, -1);
            parent->children[0] = flatfs_address_to_offset(sb, sn);
        }
        if (ty == BRT_INTN) {
            i++;
        }
    }

    /* > div: right node */
    vmp = BRT_VALIDMAP_EMPTY;
    victims = 0;
    pslot = &rn->slots[tkn - i - 1];
    memcpy(tmp_slots, rn->slots, rkn * sizeof(*tmp_slots));
    for (j = tkn - 1; j >= i; j--) {
        if (entries[j].owner == rn) {
            valid_idx = tmp_slots[entries[j].idx];
        } else {
            valid_idx = find_first_zero_bit_le(&vmp, BRT_VALIDMAP_SIZE);
            rn->entries[valid_idx] = entries[j].ent;
            if (ty == BRT_INTN) {
                rn->children[valid_idx + 1] = entries[j].rc;
            }
            victims |= 1ul << valid_idx;
            __set_bit_le(valid_idx, &vmp);
            brt_nd_copy_dirty(dst, rn, j - i, entries[j].owner, entries[j].idx);
            rebalance_delta(rn, entries[j].owner, &rn->entries[valid_idx]);
        }
        if (ty == BRT_INTN) {
            rn->children[0] = entries[j].lc;
        }
        __set_bit_le(valid_idx, &vmp);
        *pslot-- = valid_idx;
    }
    if (vmp & ~BRT_VALIDMAP_ENMSK) {
        rn->validmap = vmp;
        nd_suf2pre(dst, rn);
        /* Persistent the right node. */
        flatfs_flush_buffer(&rn->cls[1], CACHELINE_SIZE, 0);
        sel_persist_ents(rn, victims);
        if (ty == BRT_INTN) {
            sel_persist_children(rn, (victims << 1) + 1);
        }
        free_right = 0;
    } else {
        /* Empty right node, free it. */
        free_right = 1;
    }

    /* Empty the @src tree, and fence */
    src->root = NULL;
    src->height = 0;
    src->ptree->root = 0;
    flatfs_flush_buffer(&src->ptree->root, sizeof(src->ptree->root), 1);

    flatfs_commit_transaction(sb, trans);

    if (free_right) {
        brt_nd_free(rt, rn);
    }

out:
    return ret;
}

int brt_absorb(brt_tree_t *dst, brt_tree_t *src) {
#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif
#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "absorb", dst);
    brbag_tree(&lb, src);
    brbag_end(&lb);
#endif
    return do_absorb(dst, src);
}

#define MAX_PARTS_N     BRT_MAX_HEIGHT

typedef struct {
    brt_ptree_t left[MAX_PARTS_N + 1];
    brt_ptree_t right[MAX_PARTS_N + 1];
} __packed tree_parts_t;

static void postprocess_part(brt_tree_t *tree, brt_ptree_t **pt,
                             brt_node_t *root) {
    struct super_block *sb = tree->sbi->super;

    if (unlikely(!root)) {
        return;
    }

    nd_suf2pre(tree, root);

    tree_max(tree, root)->next = BRT_NOFF;

    (*pt)->state = BRT_PT_INTACT;
    (*pt)->root = flatfs_address_to_offset(sb, root);
    (*pt)++;
}

/*
 * The function is hard to understand, but I haven't figured
 * out how to explain it clearly so far...
 */
static brt_node_t *cut_node(flatfs_transaction_t *trans,
                            brt_tree_t *tree, brt_node_t *node,
                            brt_ptree_t **lp, brt_ptree_t **rp,
                            brt_key_t lobnd) {
    struct super_block *sb = tree->sbi->super;

    unsigned long ent_victims, chld_victims;
    brt_ndtype_t ty = brt_nd_type(node);
    u8 nk = brt_nd_nkeys(node);
    brt_node_t *next, *root;
    u8 pos, lnc, rnc, i, j;
    int is_not_equal;

    pos = keys_lowerbound(tree, node, lobnd, &is_not_equal);

    lnc = pos;
    rnc = nk - pos;
    next = NULL;
    if (ty == BRT_LEAF) {
        rnc++;
        lnc++;
    } else if (!is_not_equal) {
        lnc++;
    } else {
        next = brt_nd_chld_n(tree, node, pos);
    }

    /* Generate right node. */
    root = NULL;
    if (rnc == 1) {
        if (ty == BRT_INTN) {
            root = brt_nd_chld_n(tree, node, nk);
        }
    } else if (rnc > 1) {
        root = brt_nd_new(tree, ty);
        dup_node_pre(tree, root, node);
        ent_victims = chld_victims = 0;
        for (i = nk - rnc + 1, j = 0; i < nk; i++, j++) {
            u8 k = node->slots[i];
            __set_bit_le(k, &root->validmap);
            root->entries[k] = node->entries[k];
            root->slots[j] = k;
            ent_victims |= 1ul << k;
            if (ty == BRT_INTN) {
                root->children[k + 1] = node->children[k + 1];
                chld_victims |= 1ul << (k + 1);
            }
            brt_nd_copy_dirty(tree, root, j, node, i);
        }
        if (ty == BRT_INTN) {
            __le64 chld = node->children[node->slots[pos] + 1];
            root->children[0] = chld;
            chld_victims |= 1;
        }
        flatfs_flush_buffer(&root->cls[0], 3 * CACHELINE_SIZE, 0);
        sel_persist_ents(root, ent_victims);
        sel_persist_children(root, chld_victims);
    }
    postprocess_part(tree, rp, root);

    /* Generate left node. */
    root = NULL;
    if (lnc == 1) {
        if (ty == BRT_INTN) {
            root = brt_nd_chld_n(tree, node, 0);
        }
    } else if (lnc > 1) {
        /* We just need to change the validmap. */
        root = node;
        flatfs_add_logentry(sb, trans,
                            &root->validmap, sizeof(root->validmap), LE_DATA);
        for (i = lnc - 1; i < nk; i++) {
            __clear_bit_le(root->slots[i], &root->validmap);
        }
        flatfs_flush_buffer(&root->validmap, sizeof(root->validmap), 0);
    }
    postprocess_part(tree, lp, root);

    return next;
}

static int teardown(brt_tree_t *tree, brt_key_t lobnd) {
    struct super_block *sb = tree->sbi->super;

    DEFINE_TRANS_ON_STACK(trans);
    unsigned long blknr, blkoff;
    brt_ptree_t *lp, *rp;
    tree_parts_t *parts;
    brt_node_t *n;
    int ret = 0;

    trans = flatfs_init_transaction(sb, trans, MAX_PARTS_N + 1);

    flatfs_add_logentry(sb, trans, tree->ptree, sizeof(brt_ptree_t), LE_DATA);

    /* Create a 4K zeroed block for parts. */
    ret = flatfs_new_block(sb, &blknr, FLATFS_BLOCK_TYPE_4K, 0);
    if (unlikely(ret < 0)) {
        goto out;
    }
    blkoff = flatfs_get_block_off(sb, blknr, FLATFS_BLOCK_TYPE_4K);
    parts = flatfs_get_block(sb, blkoff);
    memset(parts, 0, sizeof(*parts));

    /* Split the root node, and recur if necessary. */
    lp = parts->left;
    rp = parts->right;
    n = tree->root;
    for (; n; n = cut_node(trans, tree, n, &lp, &rp, lobnd));

    tree->ptree->state = BRT_PT_BROKEN;
    tree->ptree->parts = flatfs_address_to_offset(sb, parts);

    flatfs_commit_transaction(sb, trans);

out:
    return ret;
}

static int reassemble(struct super_block *sb,
                      brt_tree_t *assembled, brt_ptree_t *parts) {
    brt_ptree_t *part;
    brt_tree_t tmp;
    int ret = 0;
    size_t i;

    part = &parts[MAX_PARTS_N];
    part->state = BRT_PT_INTACT;
    BUG_ON(part->root != 0);
    brt_pin(assembled, part, FLATFS_SB(sb));

    for (i = MAX_PARTS_N - 1; i != -1; i--) {
        part = &parts[i];
        if (part->root == 0) {
            continue;
        }
        BUG_ON(part->state != BRT_PT_INTACT);

        brt_pin(&tmp, part, FLATFS_SB(sb));

#ifdef CONFIG_FLATFS_DEBUG
        printk("flatfs: ======================== reassemble:\n");
        brt_dump(&tmp);
#endif

        ret = do_absorb(assembled, &tmp);
        if (unlikely(ret < 0)) {
            goto out;
        }
        BUG_ON(!brt_is_empty(&tmp));
    }

out:
    return ret;
}

int brt_release(brt_tree_t *dst, brt_tree_t *src, brt_key_t lobnd) {
    struct super_block *sb = dst->sbi->super;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

    DEFINE_TRANS_ON_STACK(trans);
    unsigned long blknr, blkoff;
    tree_parts_t *parts;
    brt_tree_t r, l;
    int ret;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "release", dst);
    brbag_tree(&lb, src);
    brbag_fastr(&lb, lobnd);
    brbag_end(&lb);
#endif

    BUG_ON(!brt_is_empty(dst));

    /* 1. Teardown */
    ret = teardown(src, lobnd);
    if (unlikely(ret < 0)) {
        goto out;
    }

    /* 2. Reassemble */
    parts = flatfs_offset_to_address(sb, src->ptree->parts);
    ret = reassemble(sb, &l, parts->left);
    if (unlikely(ret < 0)) {
        goto out;
    }
    ret = reassemble(sb, &r, parts->right);
    if (unlikely(ret < 0)) {
        goto out;
    }

    /* 3. Activate */
    trans = flatfs_init_transaction(sb, trans, 2);
    flatfs_add_logentry(sb, trans, src->ptree, sizeof(brt_ptree_t), LE_DATA);
    flatfs_add_logentry(sb, trans, dst->ptree, sizeof(brt_ptree_t), LE_DATA);
    *src->ptree = *l.ptree;
    src->root = l.root;
    src->height = l.height;
    *dst->ptree = *r.ptree;
    dst->root = r.root;
    dst->height = r.height;
    flatfs_flush_buffer(src->ptree, sizeof(brt_ptree_t), 0);
    flatfs_flush_buffer(dst->ptree, sizeof(brt_ptree_t), 1);
    flatfs_commit_transaction(sb, trans);

    blkoff = flatfs_address_to_offset(sb, parts);
    blknr = flatfs_get_blocknr(sb, blkoff, FLATFS_BLOCK_TYPE_4K);
    flatfs_free_block(sb, blknr, FLATFS_BLOCK_TYPE_4K);

out:
    return ret;
}

static void node_chgpre(brt_tree_t *tree, brt_node_t *node,
                        brt_chgpre_opt_t *opt) {
    struct super_block *sb = tree->sbi->super;

    void *dst_path_pre_buf, *dst_perm_pre_buf = NULL;
    brt_ndtype_t ty = brt_nd_type(node);
    fastr_t dst_path_pre, dst_perm_pre;
    fastr_t src_path_pre, src_perm_pre;
    fastr_t src_path_pre_old;
    __le64 new_pre_buf;
    size_t off;
    int ret;

    /* deal with path */
    src_path_pre = brt_nd_path_pre(tree, node);
    src_path_pre_old = fastr_slice_before(src_path_pre, opt->src.len);
    BUG_ON(fastr_strcmp(src_path_pre_old, opt->src) != 0);
    dst_path_pre_buf = brt_entbuf_new(tree, node, (int) BRT_NDPRE_INIT_SIZE * 2);
    dst_path_pre = fastr(dst_path_pre_buf, 0);
    fastr_shrink_pre(&src_path_pre, opt->src.len);
    fastr_append(&dst_path_pre, opt->dst);
    fastr_append(&dst_path_pre, src_path_pre);

    /* deal with perm */
    if (ty == BRT_LEAF) {
        src_perm_pre = brt_nd_perm_pre(tree, node);
        dst_perm_pre_buf = dst_path_pre_buf + BRT_NDPRE_INIT_SIZE;
        dst_perm_pre = ppcs_from_narr(dst_perm_pre_buf, 0);
        BUG_ON(get_path_depth(opt->dst) != ppcs_depth(opt->pppcs) +
                                           ppc_get_dep(opt->pppc) - 1);
        ppcs_append(&dst_perm_pre, opt->pppcs, 0, ppcs_depth(opt->pppcs));
        ppcs_append_ppc(&dst_perm_pre, opt->pppc);
        off = ppcs_shrink_pre_oop(&src_perm_pre, get_path_depth(opt->src) + 1);
        if (likely(!ppcs_is_empty(src_perm_pre))) {
            off = ppc_get_dep(src_perm_pre.words[0]) - off;
            ppcs_append(&dst_perm_pre, src_perm_pre,
                        off, ppcs_depth(src_perm_pre) - off);
        }
    }

    /* Persistent the path/perm prefix buffer. */
    flatfs_flush_buffer(dst_path_pre_buf, dst_path_pre.len, 0);
    if (ty == BRT_LEAF) {
        flatfs_flush_buffer(dst_perm_pre_buf, dst_perm_pre.len, 0);
    }

    /* Make room for recovery. */
    new_pre_buf = flatfs_address_to_offset(sb, dst_path_pre_buf);
    ret = cmpxchg_double(&node->prestorer, &node->pre_buf,
                         node->prestorer, node->pre_buf,
                         node->pre_buf, new_pre_buf);
    BUG_ON(!ret);
    flatfs_flush_buffer(&node->cls[0], CACHELINE_SIZE, 1);

    /* TODO: Crash consistency of it */
    node->delta_len += dst_path_pre.len - node->pre_len;
    node->pre_len = cpu_to_le16(dst_path_pre.len);
    node->perm_pre_len = cpu_to_le16(dst_perm_pre.len);
    brt_nd_all_dirty(tree, node);
    flatfs_flush_buffer(&node->cls[1], CACHELINE_SIZE, 0);
}

static void _brt_chgpre(brt_tree_t *tree, brt_node_t *node,
                        brt_chgpre_opt_t *opt) {
    u8 i, nk = brt_nd_nkeys(node);
    node_chgpre(tree, node, opt);
    if (brt_nd_type(node) == BRT_INTN) {
        for (i = 0; i <= nk; i++) {
            _brt_chgpre(tree, brt_nd_chld_n(tree, node, i), opt);
        }
    }
}

void brt_chgpre(brt_tree_t *tree, brt_chgpre_opt_t *opt) {
#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif
#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "chgpre", tree);
    brbag_chgpre_opt(&lb, opt);
    brbag_end(&lb);
#endif
    tree->ptree->in_chgpre = 1;
    flatfs_flush_buffer(&tree->ptree->in_chgpre,
                        sizeof(tree->ptree->in_chgpre), 1);
    _brt_chgpre(tree, tree->root, opt);
    tree->ptree->in_chgpre = 0;
    flatfs_flush_buffer(&tree->ptree->in_chgpre,
                        sizeof(tree->ptree->in_chgpre), 0);
}

void free_subtree(brt_tree_t *tree, brt_node_t *node) {
    struct super_block *sb = tree->sbi->super;
    u8 i, nk = brt_nd_nkeys(node);
    /* Free all the entries of the @node. */
    for (i = 0; i < nk; i++) {
        void *ebuf = brt_nd_entbuf(tree, node, brt_nd_ent_n(tree, node, i));
        brt_entbuf_free(sb, ebuf);
    }
    /* Free all the child nodes. */
    if (brt_nd_type(node) == BRT_INTN) {
        for (i = 0; i <= nk; i++) {
            free_subtree(tree, brt_nd_chld_n(tree, node, i));
        }
    }
    /* Free the node itself. */
    node_kill(tree, node);
}

void brt_clear(brt_tree_t *tree) {
    /* Clear the tree */
    /* TODO: Crash consistency of @brt_clear */
    brt_node_t *root = tree->root;
    BUG_ON(tree->ptree->state != BRT_PT_INTACT);
    tree->ptree->state = BRT_PT_NONE;
    tree->ptree->in_chgpre = 0;
    tree->ptree->root = 0;
    free_subtree(tree, root);
}

static void verify_node(brt_tree_t *tree, brt_node_t *node) {
    brt_node_t *last_leaf = NULL, *orig_root;
    orig_root = tree->root;
    /* fake the root pointer */
    tree->root = node;
    _verify_node(tree, node, "\1", "\255", &last_leaf);
    tree->root = orig_root;
}

void brt_verify(brt_tree_t *tree) {
    brt_node_t *last_leaf = NULL;

#ifdef CONFIG_FLATFS_DEBUG
    printk("flatfs_bplus_verifier: Verifying the structure of B+ Tree\n");
#endif

    if (unlikely(brt_is_empty(tree))) {
        return;
    }

    /* 1. Verify nodes */
    _verify_node(tree, tree->root, "\1", "\255", &last_leaf);

    /* 2. Verify tree->height field */
    if (tree->height != get_height(tree)) {
        printk("flatfs_bplus_verifier: "
               "bplus_tree->height verification failed!\n");
        BUG();
    }

    /* 3. Verify max leaf node's next pointer */
    if (tree_max(tree, tree->root)->next != BRT_NOFF) {
        printk("flatfs_bplus_verifier: "
               "The max leaf node's next pointer is not BRT_NOFF!\n");
        BUG();
    }
}

static void dump_node(brt_tree_t * tree, brt_node_t* node) {
    struct super_block *sb = tree->sbi->super;

    int i, num_keys = brt_nd_nkeys(node);

    printk("node [%lx]\n", (unsigned long) node);
    for (i = 0; i < num_keys; i++) {
        printk("key [%d]: {%.*s}%.*s\n", i,
               FASTR_FMT(brt_nd_path_pre(tree, node)),
               FASTR_FMT(brt_nd_path_suf_n(tree, node, i)));

        if (brt_nd_type(node) == BRT_LEAF) {
            ppcs_t perm_suf;
            size_t perm_suf_off;

            printk("pst [%d]: {", i);
            ppcs_print(brt_nd_perm_pre(tree, node), 0);
            printk(KERN_CONT "}");

            perm_suf = brt_nd_perm_suf_n(tree, node, i, &perm_suf_off);
            ppcs_print(perm_suf, perm_suf_off);
            printk(KERN_CONT "\n");
        }
    }

    if (brt_nd_type(node) == BRT_INTN) {
        for (i = 0; i < num_keys + 1; i++) {
            printk("children[%d]: %lx\n",
                   i, (unsigned long) brt_nd_chld_n(tree, node, i));
        }
    } else {
        printk("next: [%lx]\n", node->next != BRT_NOFF
               ? (unsigned long) flatfs_offset_to_address(sb, node->next)
               : 0ul);
    }
}

void brt_dump(brt_tree_t *tree) {
    struct fifo {
        brt_node_t *node;
        struct fifo *next;
        int dep;
    };

    struct fifo *fifo = kmalloc(sizeof(*fifo), GFP_KERNEL);
    struct fifo *tail = fifo, *tmp;

    size_t i;

    if (unlikely(!tree->root)) {
        return;
    }

    fifo->node = tree->root;
    fifo->next = NULL;
    fifo->dep = 0;

    while (fifo) {
        printk("========= %s node %lx (dep: %d) =========\n",
               brt_nd_type(fifo->node) == BRT_INTN ? "internal" : "leaf",
               (unsigned long) fifo->node, fifo->dep);

        dump_node(tree, fifo->node);
        printk("common prefix len: %hu\n", fifo->node->pre_len);

        if (brt_nd_type(fifo->node) == BRT_INTN) {
            for (i = 0; i <= brt_nd_nkeys(fifo->node); i++) {
                struct fifo *cur = kmalloc(sizeof(*cur), GFP_KERNEL);
                cur->dep = fifo->dep + 1;
                cur->node = brt_nd_chld_n(tree, fifo->node, i);
                cur->next = NULL;
                tail = tail->next = cur;
            }
        }

        tmp = fifo;
        fifo = fifo->next;
        kfree(tmp);
    }
}

static size_t calc_avg_suf_len(brt_tree_t *tree) {
    struct super_block *sb = tree->sbi->super;

    brt_node_t *node = tree->root;
    size_t sum = 0, nr = 0;
    __le64 off;

    while (brt_nd_type(node) == BRT_INTN) {
        node = flatfs_offset_to_address(sb, node->children[0]);
    }

    do {
        int num_keys = brt_nd_nkeys(node), i;

        for (i = 0; i < num_keys; i++) {
            sum += brt_nd_path_suf_n(tree, node, i).len;
            nr++;
        }

        off = node->next;
        node = flatfs_offset_to_address(sb, off);
    } while (off != BRT_NOFF);

    return sum / nr;
}

void brt_stat(brt_tree_t *tree, brt_stat_t *stat) {
    stat->height = tree->height;
    stat->avg_suf_len = calc_avg_suf_len(tree);
}

static void it_fetch_prefix(brt_it_t *it) {
    fastr_t pre = brt_nd_path_pre(it->tree, it->cur);
    fastr_clear(&it->pathname);
    fastr_append(&it->pathname, pre);
}

static void it_fetch_suffix(brt_it_t *it) {
    fastr_t pre = brt_nd_path_pre(it->tree, it->cur);
    fastr_t suf = brt_nd_path_suf_n(it->tree, it->cur, it->cur_pos);
    fastr_reserve_pre(&it->pathname, pre.len);
    fastr_append(&it->pathname, suf);
}

static void it_carry(brt_it_t *it) {
    struct super_block *sb = it->tree->sbi->super;
    brt_node_t *old = it->cur;
    if (unlikely(!old)) {
        return;
    }
    if (it->cur_pos >= brt_nd_nkeys(it->cur)) {
        if (unlikely(it->cur->next == BRT_NOFF)) {
            it->cur = NULL;
        } else {
            it->cur = flatfs_offset_to_address(sb, it->cur->next);
            read_lock(it->cur->lock);
        }
        read_unlock(old->lock);
        it->cur_pos = 0;
        if (likely(it->cur)) {
            it_fetch_prefix(it);
        }
    }
}

void brt_scan(brt_tree_t *tree, brt_it_t *it, brt_key_t start, brt_key_t end) {
    int is_not_equal;
    it->tree = tree;
    it->start_pos = 0;
    it->start = lowerbound(tree, start, &it->start_pos, &is_not_equal);
    it->pathname = fastr(kmalloc(PATH_MAX, GFP_ATOMIC), 0);
    it->cur = it->start;
    it->cur_pos = it->start_pos;
    if (likely(it->cur)) {
        it_fetch_prefix(it);
    }
}

int brt_it_next(brt_it_t *it, brt_qr_t *ent) {
    brt_ent_t *tent;
    int ret = 0;

    it_carry(it);
    if (unlikely(!it->cur)) {
        ret = -ENOENT;
        goto out;
    }
    it_fetch_suffix(it);

    BUG_ON(ent->mode != BRT_QR_RAW);
    ent->path = it->pathname;
    tent = brt_nd_ent_n(it->tree, it->cur, it->cur_pos);
    ent->inode = brt_nd_val_n(it->tree, it->cur, it->cur_pos);
    ent->valp = (__le64 *) brt_nd_val(it->tree, it->cur, tent);

    it->cur_pos++;

out:
    return ret;
}

void brt_it_close(brt_it_t *it) {
    if (likely(it->cur)) {
        read_unlock(it->cur->lock);
    }
    kfree(it->pathname.chars);
}

/* Recover the node from an range update. */
static void node_recover(brt_node_t *node) {
    int ret;
    if (node->prestorer != BRT_NOFF) {
        /* The node has been updated. Undo it. */
        ret = cmpxchg_double(&node->prestorer, &node->pre_buf,
                             node->prestorer, node->pre_buf,
                             BRT_NOFF, node->prestorer);
        BUG_ON(!ret);
        flatfs_flush_buffer(&node->cls[0], CACHELINE_SIZE, 1);
    }
}

static void _tree_recover(brt_tree_t *tree, brt_node_t *node) {
    u8 i, nk = brt_nd_nkeys(node);
    node_recover(node);
    if (brt_nd_type(node) == BRT_INTN) {
        for (i = 0; i <= nk; i++) {
            _tree_recover(tree, brt_nd_chld_n(tree, node, i));
        }
    }
}

static void tree_recover(brt_tree_t *tree) {
    if (!tree->ptree->in_chgpre) {
        return;
    }
    _tree_recover(tree, tree->root);
}

static void merge_tree_parts(struct super_block *sb, brt_ptree_t *ptree) {
    brt_tree_t assembled;
    if (ptree->state == BRT_PT_INTACT) {
        /* No need to fix an intact tree. */
        return;
    }
    /* Merge all the tree parts. */
    reassemble(sb, &assembled, flatfs_offset_to_address(sb, ptree->parts));
    ptree->root = flatfs_address_to_offset(sb, assembled.root);
    ptree->state = BRT_PT_INTACT;
    flatfs_flush_buffer(ptree, sizeof(*ptree), 1);
}

void brt_recover_all(struct super_block *sb) {
    struct flatfs_super_block *super = flatfs_get_super(sb);
    size_t n = sizeof(super->trees) / sizeof(*super->trees);
    brt_ptree_t *pt = super->trees;
    brt_tree_t tree, first;
    int total = 0;

    /* Merge tree parts and undo unfinished chgpre (range update). */
    while (n--) {
        if (pt->state != BRT_PT_NONE) {
            merge_tree_parts(sb, pt);
            brt_pin(&tree, pt, FLATFS_SB(sb));
            tree_recover(&tree);
            total++;
        }
        pt++;
    }

    /* Merge all the trees to the first. */
    if (total > 1) {
        pt = super->trees;
        brt_pin(&first, pt, FLATFS_SB(sb));
        pt++;
        n = sizeof(super->trees) / sizeof(*super->trees) - 1;
        while (n--) {
            if (pt->state != BRT_PT_NONE) {
                brt_pin(&tree, pt, FLATFS_SB(sb));
                brt_absorb(&first, &tree);
            }
            pt++;
        }
    }
}
