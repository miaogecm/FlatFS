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

#include <linux/kernel.h>
#include <linux/sort.h>
#include <linux/err.h>

#include "../flatfs.h"
#include "ndcache.h"
#include "node.h"
#include "bag.h"
#include "ndcache.h"

#define MAX_NR_POOLS        64

struct sok_pool_pool {
    DECLARE_BITMAP(bitmap, MAX_NR_POOLS);
    sok_pool_t pools[MAX_NR_POOLS];
};

static inline sok_pool_t *alloc_pool(void) {
    struct sok_pool_pool *sok_pool_pool = current->sok_pool_pool;
    int pos;
    if (unlikely(!sok_pool_pool)) {
        sok_pool_pool = kmalloc(sizeof(struct sok_pool_pool), GFP_ATOMIC);
        memset(sok_pool_pool, 0, sizeof(struct sok_pool_pool));
        current->sok_pool_pool = sok_pool_pool;
    }
    pos = find_first_zero_bit(sok_pool_pool->bitmap, MAX_NR_POOLS);
    BUG_ON(pos >= MAX_NR_POOLS);
    bitmap_set(sok_pool_pool->bitmap, pos, 1);
    return &sok_pool_pool->pools[pos];
}

static inline sok_pool_t *get_pool(brt_tree_t *tree, brt_node_t *node) {
    sok_pool_t *pool = alloc_pool();
    brt_node_get_pool(pool, tree, node);
    return pool;
}

static inline void put_pool(sok_pool_t *pool) {
    bitmap_clear(current->sok_pool_pool->bitmap, pool - current->sok_pool_pool->pools, 1);
}

static inline void kill_pool(sok_pool_t *pool) {
    brt_node_put(pool);
    put_pool(pool);
}

static int get_height(brt_tree_t *tree) {
    brt_node_t *root = tree->root;
    sok_pool_t *pool;
    int height = 0;
    if (unlikely(!root)) {
        goto out;
    }
    while (!__brt_node_is_leaf(tree, root)) {
        height++;
        pool = get_pool(tree, root);
        root = brt_node_get_child(pool, 0);
        put_pool(pool);
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
    rwlock_init(&dst->root_lock);

    FLATFS_INFO(BRTREE, "Created In-DRAM BrTree %lx from In-NVM BrTree %lx."
                        " root=%lx height=%d\n",
                        (ulong) dst, (ulong) src, (ulong) dst->root, dst->height);
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

struct dnode_key {
    fastr_t suf;
    int eh;
};

#define CMP(a, b)   fastr_strcmp(((struct dnode_key *) (a))->suf, ((struct dnode_key *) (b))->suf)
#define SWAP(a, b)  do {                                    \
    struct dnode_key tmp = *(struct dnode_key *) (a);       \
    *(struct dnode_key *) (a) = *(struct dnode_key *) (b);  \
    *(struct dnode_key *) (b) = tmp;                        \
} while (0)
#define ELEM_SIZE   sizeof(struct dnode_key)
#include "sort.h"

static inline fastr_t dnode_find_split_key(sok_pool_t *pool, unsigned long *rehmap, fastr_t *laddon, fastr_t *raddon) {
    fastr_t cut, pre = brt_node_get_prefix(pool);
    struct dnode_key keys[BRT_MAX_NR_ENT];
    DECLARE_BITMAP(ehmap, BRT_MAX_NR_ENT);
    size_t laddon_len, raddon_len;
    int eh, nr_ents = 0, pos, i;

    sok_prefetch(pool);

    brt_node_get_ehs(pool, ehmap);
    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        keys[nr_ents].suf = brt_node_get_suffix(pool, brt_node_get_entry(pool, eh));
        keys[nr_ents].eh = eh;
        nr_ents++;
    }

    _quicksort(keys, nr_ents);

    pos = nr_ents / 2;

    bitmap_zero(rehmap, BRT_MAX_NR_ENT);
    for (i = pos; i < nr_ents; i++) {
        bitmap_set(rehmap, keys[i].eh, 1);
    }

    laddon_len = fastr_prefix_len(keys[0].suf, keys[pos - 1].suf);
    raddon_len = fastr_prefix_len(keys[pos].suf, keys[nr_ents - 1].suf);

    *laddon = fastr_slice_before(keys[0].suf, laddon_len);
    *raddon = fastr_slice_before(keys[pos].suf, raddon_len);

    cut = fastr(kmalloc(pre.len + keys[pos].suf.len, GFP_ATOMIC), 0);
    fastr_append(&cut, pre);
    fastr_append(&cut, keys[pos].suf);

    for (i = 1; i < pos; i++) {
        brt_node_put_suffix(pool, keys[i].suf);
    }
    for (i = pos + 1; i < nr_ents; i++) {
        brt_node_put_suffix(pool, keys[i].suf);
    }

    return cut;
}

static fastr_t dnode_split(sok_pool_t *parent, int child_pos, sok_pool_t *left, sok_pool_t *right, sok_pool_t *src) {
    DECLARE_BITMAP(lehmap, BRT_MAX_NR_ENT);
    DECLARE_BITMAP(rehmap, BRT_MAX_NR_ENT);
    fastr_t cut, laddon, raddon;

    cut = dnode_find_split_key(src, rehmap, &laddon, &raddon);

    brt_node_get_ehs(src, lehmap);
    bitmap_xor(lehmap, lehmap, rehmap, BRT_MAX_NR_ENT);

    brt_node_copy(right, src, rehmap, raddon, brt_node_get_next(src), cut, brt_node_get_rfence(src->tree, src->node));
    brt_node_copy(left, src, lehmap, laddon, right->node, brt_node_get_lfence(src->tree, src->node), cut);

    brt_node_set_child(parent, child_pos, left->node);
    brt_node_insert(parent, child_pos, FASTR_NULL, cut, (unsigned long) right->node);

    brt_node_put_suffix(src, laddon);
    brt_node_put_suffix(src, raddon);

    return cut;
}

static fastr_t inode_split(sok_pool_t *parent, int child_pos, sok_pool_t *dst, sok_pool_t *src) {
    int nr_ents = brt_node_nr_entries(src), pos = nr_ents / 2, i;
    fastr_t suf, cut, pre = brt_node_get_prefix(src);
    DECLARE_BITMAP(rehmap, BRT_MAX_NR_ENT);
    brt_node_t *child;

    suf = brt_node_get_suffix(src, brt_node_get_entry(src, pos));

    bitmap_zero(rehmap, BRT_MAX_NR_ENT);
    bitmap_set(rehmap, pos + 1, nr_ents - pos - 1);

    child = brt_node_get_child(src, pos + 1);
    brt_node_copy(dst, src, rehmap, FASTR_NULL, child, FASTR_NULL, FASTR_NULL);
    for (i = pos; i < nr_ents; i++) {
        brt_node_remove(src, pos);
    }

    brt_node_suf2pre(src);
    brt_node_suf2pre(dst);

    brt_node_insert(parent, child_pos, pre, suf, (unsigned long) dst->node);

    cut = fastr(kmalloc(pre.len + suf.len, GFP_ATOMIC), 0);
    fastr_append(&cut, pre);
    fastr_append(&cut, suf);

    brt_node_put_suffix(src, suf);

    return cut;
}

static inline fastr_t split(sok_pool_t *parent, int child_pos,
                            sok_pool_t **left, sok_pool_t **right, sok_pool_t *pool) {
    fastr_t cut;

    *right = alloc_pool();
    if (brt_node_is_leaf(pool)) {
        *left = alloc_pool();
        cut = dnode_split(parent, child_pos, *left, *right, pool);
        brt_node_list_insert(pool->tree, pool->node, (*left)->node);
        brt_node_list_insert(pool->tree, pool->node, (*right)->node);
        brt_node_list_remove(pool->tree, pool->node);
        brt_node_lock((*left)->tree, (*left)->node, true);
        brt_node_invalidate(pool);
        brt_node_unlock(pool->tree, pool->node, true);
        kill_pool(pool);
    } else {
        *left = pool;
        cut = inode_split(parent, child_pos, *right, pool);
    }

    return cut;
}

/*
 * BrTree walk functions
 * BrTree supports three kinds of walk:
 * (1) Fast walk (use ndcache)
 * (2) Crabbing walk (for get/del)
 * (3) Preemptively-split walk (for add)
 */

enum tree_walk_type {
    TREE_WALK_READ,
    TREE_WALK_ADD,
    TREE_WALK_DEL
};

bool ndcache_enable = true;

static inline void tree_walk_cache(brt_tree_t *tree, brt_node_t *node) {
    if (likely(tree->ndc)) {
        ndcache_insert(tree, node);
    }
}

static inline void vlock(brt_vnode_t *vnode, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        brt_vnode_lock(vnode, false);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        brt_vnode_lock(vnode, true);
        break;
    default:
        BUG();
    }
}

static inline void vunlock(brt_vnode_t *vnode, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        brt_vnode_unlock(vnode, false);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        brt_vnode_unlock(vnode, true);
        break;
    default:
        BUG();
    }
}

static sok_pool_t *tree_walk_fastpath(brt_tree_t *tree, fastr_t key, enum tree_walk_type type) {
    struct ndcache_entry *entry;
    sok_pool_t *pool = NULL;
    brt_node_t *node;
    bool good;

    if (unlikely(!ndcache_enable)) {
        goto out;
    }

    if (unlikely(!tree->ndc)) {
        goto out;
    }

    preempt_disable();

    ndcache_for_each_entry(tree->ndc, entry, {
        brt_vnode_prefetch(entry->vnode);

        vlock(entry->vnode, type);

        if (!brt_vnode_range_contains(entry->vnode, key)) {
            goto try_next;
        }

        node = entry->node;

        BUG_ON(!__brt_node_is_leaf(tree, node));

        pool = get_pool(tree, node);

        switch (type) {
            case TREE_WALK_ADD:
                /* Ensure no split will happen, otherwise the parent node should be locked. */
                good = brt_node_nr_entries(pool) < BRT_MAX_NR_ENT;
                break;
            case TREE_WALK_DEL:
                /* Ensure no node free will happen, otherwise the parent node should be locked. */
                good = brt_node_nr_entries(pool) > 1;
                break;
            case TREE_WALK_READ:
                good = true;
                break;
            default:
                BUG();
        }

        if (likely(good)) {
            ndcache_access(tree, entry, node);
            break;
        }

        put_pool(pool);
        pool = NULL;

try_next:
        vunlock(entry->vnode, type);
    });

    preempt_enable();

out:
    return pool;
}

static inline void walk_node_lock(brt_tree_t *tree, brt_node_t *node, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        brt_node_lock(tree, node, false);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        brt_node_lock(tree, node, true);
        break;
    default:
        BUG();
    }
}

static inline void walk_node_unlock(brt_tree_t *tree, brt_node_t *node, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        brt_node_unlock(tree, node, false);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        brt_node_unlock(tree, node, true);
        break;
    default:
        BUG();
    }
}

static inline brt_node_t *walk_node_get_and_lock_next(sok_pool_t *pool, enum tree_walk_type type) {
    brt_node_t *next;

    switch (type) {
    case TREE_WALK_READ:
        next = brt_node_get_and_lock_next(pool, false);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        next = brt_node_get_and_lock_next(pool, true);
        break;
    default:
        BUG();
    }

    return next;
}

static inline void walk_root_lock(brt_tree_t *tree, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        read_lock(&tree->root_lock);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        write_lock(&tree->root_lock);
        break;
    default:
        BUG();
    }
}

static inline void walk_root_unlock(brt_tree_t *tree, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        read_unlock(&tree->root_lock);
        break;
    case TREE_WALK_ADD:
    case TREE_WALK_DEL:
        write_unlock(&tree->root_lock);
        break;
    default:
        BUG();
    }
}

struct ancestors {
    sok_pool_t *pools[BRT_MAX_HEIGHT];
    int nr;
};

static inline void crab_pool_unlock(brt_tree_t *tree, sok_pool_t *pool, enum tree_walk_type type) {
    if (pool == NULL) {
        walk_root_unlock(tree, type);
    } else {
        walk_node_unlock(tree, pool->node, type);
    }
}

static inline bool is_suspecious(sok_pool_t *pool, enum tree_walk_type type) {
    switch (type) {
    case TREE_WALK_READ:
        return false;
    case TREE_WALK_DEL:
        return brt_node_nr_entries(pool) <= (brt_node_is_leaf(pool) ? 1 : 0);
    default:
        BUG();
    }
}

static void crab_unlock_all(brt_tree_t *tree, struct ancestors *ancestors, enum tree_walk_type type) {
    int i;

    for (i = 0; i < ancestors->nr; i++) {
        crab_pool_unlock(tree, ancestors->pools[i], type);
        if (ancestors->pools[i]) {
            put_pool(ancestors->pools[i]);
        }
    }
    ancestors->nr = 0;
}

static brt_node_t *crab_get_root(struct ancestors *suspects, brt_tree_t *tree, enum tree_walk_type type) {
    brt_node_t *root;

    walk_root_lock(tree, type);
    root = tree->root;
    if (root == NULL) {
        walk_root_unlock(tree, type);
        return NULL;
    }

    suspects->pools[suspects->nr++] = NULL;

    return root;
}

static sok_pool_t *crab_get_inode_pool(struct ancestors *suspects,
                                       brt_tree_t *tree, brt_node_t *node, enum tree_walk_type type) {
    sok_pool_t *pool;

    walk_node_lock(tree, node, type);
    pool = get_pool(tree, node);

    BUG_ON(brt_node_is_leaf(pool));

    if (!is_suspecious(pool, type)) {
        crab_unlock_all(tree, suspects, type);
    }

    suspects->pools[suspects->nr++] = pool;

    return pool;
}

/* NOTE: The caller is responsible to unlock the leaf node. */
static sok_pool_t *tree_walk_crabbing(struct ancestors *suspects,
                                      brt_node_t **prev, brt_node_t **next,
                                      brt_tree_t *tree, brt_key_t key, enum tree_walk_type type) {
    sok_pool_t *pool = NULL;
    int pos, is_not_equal;
    brt_node_t *node;

    suspects->nr = 0;

    pool = tree_walk_fastpath(tree, key, type);
    if (pool) {
        goto out;
    }

    node = crab_get_root(suspects, tree, type);
    if (unlikely(!node)) {
        goto out;
    }

    while (!__brt_node_is_leaf(tree, node)) {
        pool = crab_get_inode_pool(suspects, tree, node, type);
        pos = brt_node_lowerbound(pool, key, &is_not_equal);
        if (!is_not_equal) {
            pos++;
        }
        node = brt_node_get_child(pool, pos);
    }

    walk_node_lock(tree, node, type);
    pool = get_pool(tree, node);

    if (prev) {
        *next = walk_node_get_and_lock_next(pool, type);
        *prev = brt_node_get_prev(pool);
    }

    if (!is_suspecious(pool, type)) {
        crab_unlock_all(tree, suspects, type);
    }

    tree_walk_cache(tree, pool->node);

out:
    return pool;
}

/* NOTE: The caller is responsible to unlock the leaf node. */
static sok_pool_t *tree_walk_preemptively_split(brt_tree_t *tree, fastr_t key) {
    struct super_block *sb = tree->sbi->super;

    brt_node_t *child, *prev = NULL, *next = NULL;
    sok_pool_t *pool, *parent, *new_root;
    int is_not_equal, pos;

    pool = tree_walk_fastpath(tree, key, TREE_WALK_ADD);
    if (pool) {
        goto out;
    }

    parent = NULL;
    pos = 0;

    /* Increase the tree height if needed. */
    new_root = NULL;
    write_lock(&tree->root_lock);
    if (unlikely(!tree->root)) {
        pool = alloc_pool();
        brt_node_create_root(pool, tree);
        brt_node_list_insert(tree, NULL, pool->node);
        new_root = pool;
        brt_node_lock(tree, pool->node, true);
    } else {
        brt_node_lock(tree, tree->root, true);
        pool = get_pool(tree, tree->root);
    }
    if (unlikely(brt_node_nr_entries(pool) == BRT_MAX_NR_ENT)) {
        /* To split the root node, we have to create a new root. */
        parent = alloc_pool();
        brt_node_create_root(parent, tree);
        new_root = parent;
        brt_node_lock(tree, parent->node, true);
    }
    if (unlikely(new_root)) {
        tree->root = new_root->node;
        tree->height++;
        if (brt_node_is_leaf(new_root)) {
            __le64 noff = flatfs_address_to_offset(sb, new_root->node);
            flatfs_memcpy_atomic(&tree->ptree->root, &noff, sizeof(noff));
            flatfs_flush_buffer(&tree->ptree->root, sizeof(noff), true);
        }
    }
    write_unlock(&tree->root_lock);

    /*
     * Find which node we insert the key into, while splitting preemptively
     * along the path.
     */
    for (;;) {
        int nkeys = brt_node_nr_entries(pool);

        /* overflow check */
        if (nkeys == BRT_MAX_NR_ENT) {
            sok_pool_t *left, *right;
            fastr_t cut;

            BUG_ON(!parent);
            cut = split(parent, pos, &left, &right, pool);

            if (brt_node_is_leaf(pool)) {
                /* Durable point of split. */
                if (prev) {
                    brt_node_persist_next(tree, prev, left->node);
                } else {
                    /* TODO: update head pointer */
                }
            }

            if (fastr_strcmp(cut, key) <= 0) {
                /*
                 * If split_key <= key, then must be in the right sibling of
                 * node. We move like a crab here.
                 */
                brt_node_lock(tree, right->node, true);
                brt_node_unlock(tree, left->node, true);
                put_pool(left);
                pool = right;
            } else {
                put_pool(right);
                pool = left;
            }

            kfree(cut.chars);
        }

        if (next) {
            brt_node_unlock(tree, next, true);
        }

        if (parent) {
            brt_node_unlock(tree, parent->node, true);
            put_pool(parent);
        }

        if (brt_node_is_leaf(pool)) {
            break;
        }

        pos = brt_node_lowerbound(pool, key, &is_not_equal);

        parent = pool;
        if (!is_not_equal) {
            pos++;
        }
        child = brt_node_get_child(pool, pos);
        brt_node_lock(tree, child, true);
        pool = get_pool(tree, child);
        if (brt_node_is_leaf(pool)) {
            next = brt_node_get_and_lock_next(pool, true);
            prev = brt_node_get_prev(pool);
        }
    }

    tree_walk_cache(tree, pool->node);

out:
    return pool;
}

int brt_get(brt_tree_t *tree, brt_qr_t *res, brt_key_t key) {
    struct ancestors suspects;
    sok_pool_t *pool;
    int ret = 0, eh;

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

    pool = tree_walk_crabbing(&suspects, NULL, NULL, tree, key, TREE_WALK_READ);
    if (unlikely(!pool)) {
        ret = -ENOENT;
        goto out;
    }

    eh = brt_node_find(pool, key);
    if (unlikely(eh < 0)) {
        ret = eh;
        goto out_unlock;
    }

    res->path = key;
    res->ppcs = PPCS_NULL;
    res->inode = brt_node_get_ino(pool, brt_node_get_entry(pool, eh));

out_unlock:
    brt_node_unlock(tree, pool->node, false);

    put_pool(pool);

out:
    return ret;
}

void brt_got(brt_tree_t *tree, brt_qr_t *res) {
    if (res->ppcs.words != res->embed_ppcs) {
        kfree(res->ppcs.chars);
    }
}

static void collapse_ancestors(brt_tree_t *tree, struct ancestors *ancestors, brt_key_t key) {
    sok_pool_t *parent = ancestors->pools[0];
    int is_not_equal, pos, i;

    if (parent) {
        pos = brt_node_lowerbound(parent, key, &is_not_equal);
        if (!is_not_equal) {
            pos++;
        }

        if (pos > 0) {
            /* TODO: update dnode rfence */
            brt_node_remove(parent, pos - 1);
        } else {
            brt_node_set_min_child(parent, brt_node_get_child(parent, 1));
            brt_node_remove(parent, 0);
        }

        brt_node_unlock(tree, parent->node, true);
        put_pool(parent);
    } else {
        /* Tree becomes empty. */
        tree->root = NULL;
        tree->height = 0;

        write_unlock(&tree->root_lock);
    }

    for (i = 1; i < ancestors->nr; i++) {
        sok_pool_t *pool = ancestors->pools[i];
        brt_node_unlock(tree, pool->node, true);
        kill_pool(pool);
    }

    ancestors->nr = 0;
}

int brt_del(brt_tree_t* tree, brt_key_t key) {
#ifdef CONFIG_FLATFS_BRBAG
    brbag_lbuf_t lb;
#endif

    brt_node_t *prev = NULL, *next = NULL;
    struct ancestors suspects;
    bool free_pool = false;
    sok_pool_t *pool;
    int ret = 0, eh;

#ifdef CONFIG_FLATFS_BRBAG
    brbag_op(&lb, "del", tree);
    brbag_fastr(&lb, key);
    brbag_end(&lb);
#endif

    FLATFS_INFO(BRTREE_DEL, "(%lu) %.*s$", key.len, FASTR_FMT(key));

    pool = tree_walk_crabbing(&suspects, &prev, &next, tree, key, TREE_WALK_DEL);
    if (unlikely(!pool)) {
        ret = -ENOENT;
        goto out;
    }

    eh = brt_node_find(pool, key);
    if (unlikely(eh < 0)) {
        ret = eh;
        goto out_unlock;
    }

    if (unlikely(brt_node_nr_entries(pool) == 1)) {
        if (prev) {
            /*
             * Now the dnode will become empty. Remove it from the dnode list.
             * This is the durable point of @brt_del operation on such case.
             */
            brt_node_persist_next(tree, prev, brt_node_get_next(pool));
        } else {
            /* TODO: change head */
        }

        brt_node_list_remove(tree, pool->node);
        if (next) {
            brt_node_unlock(tree, next, true);
        }

        collapse_ancestors(tree, &suspects, key);

        brt_node_invalidate(pool);
        free_pool = true;
    } else if (next) {
        brt_node_unlock(tree, next, true);
    }
    next = NULL;

    brt_node_remove(pool, eh);
    brt_node_commit(pool);

    brt_node_compact(pool);
    brt_node_commit(pool);

out_unlock:
    crab_unlock_all(pool->tree, &suspects, TREE_WALK_DEL);
    brt_node_unlock(pool->tree, pool->node, true);
    if (next) {
        brt_node_unlock(pool->tree, next, true);
    }

    if (!free_pool) {
        put_pool(pool);
    } else {
        kill_pool(pool);
    }

out:
    return ret;
}

static void preadd(brt_tree_t *tree, int nr) {
    struct super_block *sb = tree->sbi->super;
    /* A node split may produce 2 new nodes. */
    brt_node_prealloc(sb, nr * 2);
    brt_node_preinsert(sb, nr);
}

static long __brt_add(brt_tree_t *tree,
                      brt_key_t k, brt_val_t v,
                      ppcs_t pppcs, ppc_t pppc, bool update) {
    sok_pool_t *pool;
    long ret;
    int eh;

    preadd(tree, 1);

    pool = tree_walk_preemptively_split(tree, k);

    eh = brt_node_find(pool, k);

    if (!update) {
        /* leaf insert */
        if (likely(eh == -ENOENT)) {
            brt_node_insert(pool, -1, FASTR_NULL, k, v);
            brt_node_commit(pool);

            ret = 0;
        } else {
            ret = (long) brt_node_get_ino(pool, brt_node_get_entry(pool, eh));
        }
    } else {
        /* leaf update */
        if (likely(eh != -ENOENT)) {
            brt_node_set_val(pool, brt_node_get_entry(pool, eh), v);

            ret = 0;
        } else {
            ret = -ENOENT;
        }
    }

    brt_node_unlock(tree, pool->node, true);

    put_pool(pool);

    return ret;
}

long brt_add(brt_tree_t *tree, brt_key_t k, brt_val_t v, ppcs_t pppcs, ppc_t pppc) {
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

void brt_sok_init(struct super_block *sb);
void brt_node_init(struct super_block *sb);

static void brt_tree_init(struct super_block *sb) {
}

void brt_init(struct super_block *sb) {
    brt_sok_init(sb);
    brt_node_init(sb);
    brt_tree_init(sb);
}

void brt_deinit(struct super_block *sb) {
    /* TODO: deinit brt */
}

void brt_recover_all(struct super_block *sb) {
    /* TODO */
}

void brt_dump(brt_tree_t *tree) {
    /* TODO */
}

void brt_verify(brt_tree_t *tree) {
    /* TODO */
}

int brt_absorb(brt_tree_t *dst, brt_tree_t *src) {
    /* TODO */
    return 0;
}

int brt_release(brt_tree_t *dst, brt_tree_t *src, brt_key_t lobnd) {
    /* TODO */
    return 0;
}

void brt_chgpre(brt_tree_t *tree, brt_chgpre_opt_t *opt) {
    /* TODO */
}

void brt_clear(brt_tree_t *tree) {
    /* TODO */
}

void brt_dup(brt_tree_t *dst, brt_tree_t *src) {
    /* TODO */
}

static sok_pool_t *lowerbound(brt_tree_t *tree, fastr_t key, int *pos, int *is_not_equal) {
    struct ancestors suspects;
    sok_pool_t *pool = NULL;

    pool = tree_walk_crabbing(&suspects, NULL, NULL, tree, key, TREE_WALK_READ);
    if (unlikely(!pool)) {
        goto out;
    }

    *pos = brt_node_lowerbound(pool, key, is_not_equal);

out:
    return pool;
}

static inline void it_fetch_pathname(brt_it_t *it, char delimiter) {
    sok_entry_t ent = brt_node_get_entry_by_order(it->cur, it->cur_pos);
    size_t suf_len = brt_node_get_suffix_len(it->cur, ent), pos;
    pos = brt_node_suffix_find_first(it->cur, ent, delimiter);
    fastr_clear(&it->pathname);
    if (pos == suf_len) {
        /* Example: pre: /home/hhu/f; suf: ile */
        fastr_t pre = brt_node_get_prefix(it->cur);
        pre = fastr_slice_after(pre, (off_t) fastr_find_last(pre, delimiter) + 1);
        fastr_append(&it->pathname, pre);
    }
    brt_node_get_partial_suffix(it->cur, it->pathname.chars + it->pathname.len, ent, pos);
    it->pathname.len += pos;
}

static inline void it_set_end_pos(brt_it_t *it) {
    fastr_t end = it->end, pre;
    unsigned long mismatch;
    int is_not_equal;

    if (unlikely(!it->cur)) {
        it->end_pos = 0;
        return;
    }

    pre = brt_node_get_prefix(it->cur);

    /*
     * Fast path: All entries in node are within range.
     * pre+suf1 < pre+suf2 < pre+suf3 < ... < pre+sufn < end
     * <= pre+max(suf) < end
     * <= pre+\x7f < end
     *
     * Case 1: Prefix is not a prefix of end.
     *         pre+\x7f < end <=> pre < end
     * Case 2: Prefix is a prefix of end.
     *         pre+\x7f < end <=> \x7f < end-pre <=> false
     *
     * So the fast path case should satisfy:
     * (1) Prefix is not a prefix of end.
     * (2) pre < end
     */
    fastr_eliminate(&pre, &end);
    if ((mismatch = fastr_first_zpword(pre)) && fastr_wordcmp(mismatch, fastr_first_zpword(end)) < 0) {
        it->end_pos = brt_node_nr_entries(it->cur);
    } else {
        it->end_pos = brt_node_lowerbound(it->cur, it->end, &is_not_equal);
    }
}

void brt_scan(brt_tree_t *tree, brt_it_t *it, brt_key_t start, brt_key_t end) {
    int is_not_equal;
    it->tree = tree;
    it->pathname = fastr(__getname(), 0);
    it->end = end;
    it->cur = lowerbound(tree, start, &it->cur_pos, &is_not_equal);
    it_set_end_pos(it);
}

static inline void it_carry(brt_it_t *it) {
    sok_pool_t *old = it->cur;
    brt_node_t *next;

    if (unlikely(!old)) {
        return;
    }

    if (it->cur_pos < brt_node_nr_entries(it->cur)) {
        return;
    }

    next = brt_node_get_and_lock_next(it->cur, false);
    if (likely(next)) {
        it->cur = get_pool(it->tree, next);
        sok_prefetch(it->cur);
    } else {
        it->cur = NULL;
    }
    brt_node_unlock(it->tree, old->node, false);
    put_pool(old);

    it->cur_pos = 0;
    it_set_end_pos(it);
}

int brt_it_next(brt_it_t *it, brt_qr_t *ent, char delimiter) {
    int ret = -ENOENT;
    sok_entry_t tent;

    it_carry(it);
    if (unlikely(!it->cur)) {
        goto out;
    }

    if (it->cur_pos == it->end_pos) {
        goto out;
    }

    it_fetch_pathname(it, delimiter);

    BUG_ON(ent->mode != BRT_QR_RAW);
    ent->path = it->pathname;
    tent = brt_node_get_entry_by_order(it->cur, it->cur_pos);
    ent->inode = brt_node_get_ino(it->cur, tent);
    ent->valp = (__le32 *) brt_node_get_val_ptr(it->cur, tent);

    it->cur_pos++;

    ret = 0;

out:
    return ret;
}

void brt_it_close(brt_it_t *it) {
    if (it->cur) {
        brt_node_unlock(it->tree, it->cur->node, false);
        put_pool(it->cur);
    }
    __putname(it->pathname.chars);
}

static inline void stat_add(int dst[SOK_ENT_MAX_SEG + 1][2], int src[SOK_ENT_MAX_SEG + 1][2]) {
    int nr_segs, nr_off_pgs;
    for (nr_segs = 0; nr_segs <= SOK_ENT_MAX_SEG; nr_segs++) {
        for (nr_off_pgs = 0; nr_off_pgs < 2; nr_off_pgs++) {
            dst[nr_segs][nr_off_pgs] += src[nr_segs][nr_off_pgs];
        }
    }
}

static void stat_node(brt_stat_t *stat, sok_pool_t *pool) {
    brt_ndstat_t ndstat;
    brt_node_stat(&ndstat, pool);
    stat->nr_nodes++;
    stat->nr_entries += ndstat.nr_entries;
    stat->total_filename_len += ndstat.total_filename_len;
    stat->total_suf_len += ndstat.total_suf_len;
    stat->total_suf_cached_len += ndstat.total_suf_cached_len;
    stat->total_path_len += ndstat.total_path_len;
    stat_add(stat->nr_filename, ndstat.nr_filename);
    stat_add(stat->nr_suf, ndstat.nr_suf);
    stat_add(stat->nr_suf_cached, ndstat.nr_suf_cached);
}

void brt_stat(brt_tree_t *tree, brt_stat_t *stat) {
    struct list_head *leaf_list_head = &tree->sbi->leaf_list_head;
    brt_vnode_t *vnode;
    sok_pool_t *pool;
    memset(stat, 0, sizeof(*stat));
    stat->height = tree->height;
    list_for_each_entry(vnode, leaf_list_head, leaf_list) {
        pool = get_pool(tree, vnode->dnode);
        stat_node(stat, pool);
        put_pool(pool);
    }
}
