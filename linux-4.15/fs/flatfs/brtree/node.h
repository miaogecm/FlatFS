/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * BrTree Node Management
 */

#ifndef BRTREE_NODE_H
#define BRTREE_NODE_H

#include <linux/kernel.h>
#include <linux/rwlock_types.h>

#include "sok.h"

/* In-DRAM Index Node, 1704B, 256B aligned */
struct brt_inode_s {
    /* Metadata cacheline, 64B */
    rwlock_t lock;                    /* 8B  */
    u64      pre_buf : 40;            /* 5B  */
    u16      pre_len;                 /* 2B  */
    u16      delta_len;               /* 2B  */
    u8       slots[BRT_MAX_NR_ENT];   /* 41B */
    u64      min_child : 40;          /* 5B  */
    int      is_border : 1;           /* 1b  */
    int      nr_entries : 7;          /* 7b  */

    /* Data cachelines, 1640B */
    char     segs[BRT_IENT_NR_SEG * BRT_MAX_NR_ENT][BRT_SEG_SIZE];
} __packed;

/*
 * Fingerprint-based leaf node does not support lowerbound
 * operation. We use @lengths array to handle the semantic
 * component lookup case. But we need a general lowerbound
 * operation for @brt_lobnd and its iterator. We cache the
 * permuter in dnode, and rebuild it if dnode is modified.
 */
struct brt_perm_cache {
    /* Protected by dnode's rwlock */
    long      curr_ver;
    long      perm_ver;
    u8        perm_arr[BRT_MAX_NR_ENT];
};

/*
 * Each dnode has a corresponding in-DRAM vnode. Vnode stores
 * volatile node info like lock status and node ref count.
 */
struct brt_vnode_s {
    brt_dnode_t *dnode;

    rwlock_t lock;
    fastr_t  lfence;
    fastr_t  rfence;
    u16      refcount;
    u16      pre_len;

    struct brt_perm_cache perm_cache;

    /*
     *      prev <-> curr <-> next
     *
     * Each leaf node has a @prev and @next pointer.
     * @prev->next pointer is protected by @curr and @next lock.
     * @next->prev pointer is protected by @curr and @next lock.
     */
    struct list_head leaf_list;

    struct rcu_head rcu;
};

/* In-NVM Data Node, 3536B, 256B aligned */
struct brt_dnode_s {
    /* Metadata cachelines, 256B */
    brt_vnode_t *vnode;       /* 8B  */
    __le64 fences;            /* 8B  */
    __le64 bitmap[2];         /* 16B */
    __le64 next;              /* 8B  */
    __le16 lfence_len;        /* 2B  */
    __le16 rfence_len;        /* 2B  */
    __le16 delta_len;         /* 2B  */
    __le16 pre_len;           /* 2B  */
    __le16 forward_size;      /* 2B  */
    /* 41B */
    u8     fgprts[BRT_MAX_NR_ENT];
    /* 82B */
    u8     slots[BRT_MAX_NR_ENT][SOK_ENT_MAX_SEG];
    /* 82B */
    __le16 lengths[BRT_MAX_NR_ENT];
    char   padding[1];        /* 1B  */

    /* Data cachelines, 3280B */
    char   segs[BRT_DENT_NR_SEG * BRT_MAX_NR_ENT][BRT_SEG_SIZE];

    /* Prefix buffer */
    char   pre_buf[];
} __packed;

/* statistics of a BrTree Node */
struct brt_ndstat_s {
    int nr_entries;
    int nr_filename[SOK_ENT_MAX_SEG + 1][2];
    int nr_suf[SOK_ENT_MAX_SEG + 1][2];
    int nr_suf_cached[SOK_ENT_MAX_SEG + 1][2];
    size_t total_filename_len;
    size_t total_suf_len;
    size_t total_suf_cached_len;
    size_t total_path_len;
};

/*************************************************************
 * Access Interfaces
 *************************************************************/

#define BRT_DNODE(node)      ((brt_dnode_t *) (node))
#define BRT_INODE(node)      ((brt_inode_t *) (node))

void brt_node_get_pool(sok_pool_t *pool, brt_tree_t *tree, brt_node_t *node);
fastr_t brt_node_get_lfence(brt_tree_t *tree, brt_node_t *node);
fastr_t brt_node_get_rfence(brt_tree_t *tree, brt_node_t *node);
fastr_t brt_node_get_prefix(sok_pool_t *pool);
fastr_t brt_node_get_suffix(sok_pool_t *pool, sok_entry_t ent);
size_t brt_node_get_suffix_len(sok_pool_t *pool, sok_entry_t ent);
void brt_node_get_partial_suffix(sok_pool_t *pool, void *buf, sok_entry_t ent, size_t len);
size_t brt_node_suffix_find_first(sok_pool_t *pool, sok_entry_t haystack, char needle);
void brt_node_put_suffix(sok_pool_t *pool, fastr_t suf);
int brt_node_compare_suffix(sok_pool_t *pool, fastr_t str, sok_entry_t ent);
size_t brt_node_match_suffix(sok_pool_t *pool, fastr_t str, sok_entry_t ent);
int brt_node_lowerbound(sok_pool_t *pool, fastr_t key, int *is_not_equal);
int brt_node_find(sok_pool_t *pool, fastr_t key);
int brt_node_order_to_eh(sok_pool_t *pool, int order);
brt_node_t *brt_node_get_and_lock_next(sok_pool_t *pool, bool w);
void brt_node_stat(brt_ndstat_t *stat, sok_pool_t *pool);

static inline bool __brt_node_is_leaf(brt_tree_t *tree, brt_node_t *node) {
    return nvm_ptr(tree->sbi, node);
}

static inline bool brt_node_is_leaf(sok_pool_t *pool) {
    return pool->durable;
}

static inline int brt_node_nr_entries(sok_pool_t *pool) {
    return brt_node_is_leaf(pool) ? sok_get_nr_entries(pool) : BRT_INODE(pool->node)->nr_entries;
}

static inline rwlock_t *__brt_node_get_lock(brt_tree_t *tree, brt_node_t *node) {
    return nvm_ptr(tree->sbi, node) ? &BRT_DNODE(node)->vnode->lock : &BRT_INODE(node)->lock;
}

static inline rwlock_t *brt_node_lock(brt_tree_t *tree, brt_node_t *node, bool w) {
    rwlock_t *lock = __brt_node_get_lock(tree, node);
    if (w) {
        write_lock(lock);
    } else {
        read_lock(lock);
    }
    return lock;
}

static inline void brt_node_unlock(brt_tree_t *tree, brt_node_t *node, bool w) {
    rwlock_t *lock = __brt_node_get_lock(tree, node);
    if (w) {
        write_unlock(lock);
    } else {
        read_unlock(lock);
    }
}

static inline brt_vnode_t *brt_vnode_get(brt_node_t *node) {
    return BRT_DNODE(node)->vnode;
}

static inline rwlock_t *brt_vnode_lock(brt_vnode_t *vnode, bool w) {
    rwlock_t *lock = &vnode->lock;
    if (w) {
        write_lock(lock);
    } else {
        read_lock(lock);
    }
    return lock;
}

static inline void brt_vnode_unlock(brt_vnode_t *vnode, bool w) {
    rwlock_t *lock = &vnode->lock;
    if (w) {
        write_unlock(lock);
    } else {
        read_unlock(lock);
    }
}

static inline void brt_vnode_prefetch(brt_vnode_t *vnode) {
    prefetch(vnode + 0 * CACHELINE_SIZE);
    prefetch(vnode + 1 * CACHELINE_SIZE);
    prefetch(vnode + 2 * CACHELINE_SIZE);
    prefetch(vnode + 3 * CACHELINE_SIZE);
}

static inline bool brt_vnode_range_contains(brt_vnode_t *vnode, fastr_t key) {
    return fastr_strcmp(key, vnode->lfence) >= 0 && fastr_strcmp(key, vnode->rfence) < 0;
}

static inline sok_entry_t __brt_node_get_entry(const u8 *slot, int nr_seg) {
    sok_entry_t ent;
    int i;
    for (i = 0; i < nr_seg; i++) {
        ent.ids[i] = slot[i];
    }
    for (; i < SOK_ENT_MAX_SEG; i++) {
        ent.ids[i] = SOK_SEG_NONE;
    }
    return ent;
}

static inline sok_entry_t brt_node_get_entry(sok_pool_t *pool, int eh) {
    brt_node_t *node = pool->node;
    if (brt_node_is_leaf(pool)) {
        return __brt_node_get_entry(BRT_DNODE(node)->slots[eh], BRT_DENT_NR_SEG);
    } else {
        return __brt_node_get_entry(&BRT_INODE(node)->slots[eh], BRT_IENT_NR_SEG);
    }
}

static inline sok_entry_t brt_node_get_entry_by_order(sok_pool_t *pool, int order) {
    return brt_node_get_entry(pool, brt_node_order_to_eh(pool, order));
}

static inline brt_node_t *brt_node_child_off2ptr(sok_pool_t *pool, unsigned long off) {
    brt_inode_t *inode = BRT_INODE(pool->node);
    BUG_ON(brt_node_is_leaf(pool));
    off *= 256;
    if (inode->is_border) {
        return flatfs_offset_to_address(pool->sb, off);
    } else {
        return (brt_node_t *) (FLATFS_VADDR_PRE + off);
    }
}

static inline unsigned long brt_node_child_ptr2off(sok_pool_t *pool, brt_node_t *node) {
    brt_inode_t *inode = BRT_INODE(pool->node);
    unsigned long off;
    BUG_ON(brt_node_is_leaf(pool));
    if (inode->is_border) {
        off = flatfs_address_to_offset(pool->sb, node);
    } else {
        off = ((unsigned long) node - FLATFS_VADDR_PRE);
    }
    BUG_ON(!IS_ALIGNED(off, 256));
    off /= 256;
    return off;
}

static inline brt_node_t *brt_node_get_ent_child(sok_pool_t *pool, sok_entry_t ent) {
    unsigned long child = 0;
    sok_memcpy_from(pool, &child, ent, 6, 5);
    return brt_node_child_off2ptr(pool, child);
}

static inline brt_node_t *brt_node_get_min_child(sok_pool_t *pool) {
    return brt_node_child_off2ptr(pool, BRT_INODE(pool->node)->min_child);
}

static inline brt_node_t *brt_node_get_child(sok_pool_t *pool, int nr) {
    if (nr == 0) {
        return brt_node_get_min_child(pool);
    } else {
        return brt_node_get_ent_child(pool, brt_node_get_entry(pool, nr - 1));
    }
}

static inline ino_t brt_node_get_ino(sok_pool_t *pool, sok_entry_t ent) {
    ino_t ino = 0;
    sok_memcpy_from(pool, &ino, ent, 6, 4);
    return ino;
}

static inline void *brt_node_get_val_ptr(sok_pool_t *pool, sok_entry_t ent) {
    return sok_get_ptr(pool, ent, brt_node_is_leaf(pool) ? 10 : 11);
}

static inline void brt_node_get_ehs(sok_pool_t *pool, unsigned long *ehmap) {
    if (!brt_node_is_leaf(pool)) {
        bitmap_zero(ehmap, BRT_MAX_NR_ENT);
        bitmap_fill(ehmap, brt_node_nr_entries(pool));
    } else {
        bitmap_copy(ehmap, pool->eidmap, BRT_MAX_NR_ENT);
    }
}

static inline bool brt_node_is_border(sok_pool_t *pool) {
    BUG_ON(brt_node_is_leaf(pool));
    return BRT_INODE(pool->node)->is_border;
}

/* Make sure that both @pool and its next node are locked. Otherwise use @brt_node_get_and_lock_next. */
static inline brt_node_t *brt_node_get_next(sok_pool_t *pool) {
    brt_vnode_t *vnode = brt_vnode_get(pool->node);
    struct flatfs_sb_info *sbi = pool->tree->sbi;
    if (unlikely(list_is_last(&vnode->leaf_list, &sbi->leaf_list_head))) {
        return NULL;
    }
    return list_next_entry(vnode, leaf_list)->dnode;
}

/* Make sure that both @pool and its next node are locked. */
static inline brt_node_t *brt_node_get_prev(sok_pool_t *pool) {
    brt_vnode_t *vnode = brt_vnode_get(pool->node);
    if (unlikely(vnode->leaf_list.prev == &pool->tree->sbi->leaf_list_head)) {
        return NULL;
    }
    return list_prev_entry(vnode, leaf_list)->dnode;
}

/*************************************************************
 * Management Interfaces
 *************************************************************/

void brt_node_create_root(sok_pool_t *dst, brt_tree_t *tree);
void brt_node_copy(sok_pool_t *dst, sok_pool_t *src, unsigned long *ehmap, fastr_t addon,
                   brt_node_t *min_child_or_next, fastr_t lfence, fastr_t rfence);
int brt_node_insert(sok_pool_t *pool, int order, fastr_t pre, fastr_t suf, unsigned long val);
void brt_node_remove(sok_pool_t *pool, int eh);
void brt_node_compact(sok_pool_t *pool);
void brt_node_suf2pre(sok_pool_t *pool);

void brt_node_commit(sok_pool_t *pool);
void brt_dnode_hold(brt_tree_t *tree, brt_node_t *node);
void brt_dnode_put(brt_tree_t *tree, brt_node_t *node);
void brt_node_put(sok_pool_t *pool);
void brt_node_invalidate(sok_pool_t *pool);
void brt_node_prealloc(struct super_block *sb, int nr);
void brt_node_preinsert(struct super_block *sb, int nr);

static inline void brt_node_barrier(sok_pool_t *pool) {
    if (brt_node_is_leaf(pool)) {
        PERSISTENT_BARRIER();
    }
}

static inline void brt_node_set_val(sok_pool_t *pool, sok_entry_t ent, unsigned long val) {
    unsigned long *ptr = brt_node_get_val_ptr(pool, ent), w = *ptr;
    if (brt_node_is_leaf(pool)) {
        memcpy(&w, &val, 4);
    } else {
        val = brt_node_child_ptr2off(pool, (brt_node_t *) val);
        memcpy(&w, &val, 5);
    }
    *ptr = w;
}

static inline void brt_node_set_min_child(sok_pool_t *pool, brt_node_t *node) {
    BRT_INODE(pool->node)->min_child = brt_node_child_ptr2off(pool, node);
}

static inline void brt_node_set_child(sok_pool_t *pool, int nr, brt_node_t *node) {
    if (nr == 0) {
        brt_node_set_min_child(pool, node);
    } else {
        brt_node_set_val(pool, brt_node_get_entry(pool, nr - 1), (unsigned long) node);
    }
}

static inline void brt_node_persist_next(brt_tree_t *tree, brt_node_t *node, brt_node_t *next) {
    __le64 off = next ? flatfs_address_to_offset(tree->sbi->super, next) : BRT_NOFF;
    brt_dnode_t *dnode = BRT_DNODE(node);
    BUG_ON(!__brt_node_is_leaf(tree, node));
    flatfs_memcpy_atomic(&dnode->next, &off, sizeof(off));
    flatfs_flush_buffer(&dnode->next, sizeof(off), true);
}

static inline void brt_node_list_insert(brt_tree_t *tree, brt_node_t *before, brt_node_t *node) {
    struct list_head *beforel = before ? &brt_vnode_get(before)->leaf_list : &tree->sbi->leaf_list_head;
    struct list_head *nodel = &brt_vnode_get(node)->leaf_list;
    list_add_tail(nodel, beforel);
}

static inline void brt_node_list_remove(brt_tree_t *tree, brt_node_t *node) {
    struct list_head *nodel = &brt_vnode_get(node)->leaf_list;
    list_del(nodel);
}

#endif //BRTREE_NODE_H
