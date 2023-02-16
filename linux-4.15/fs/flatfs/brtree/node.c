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

#include <linux/fastr.h>
#include <asm/word-at-a-time.h>

#include "../flatfs.h"
#include "pslab.h"
#include "node.h"
#include "sok.h"

#define BITMAP_LEN       (SOK_SEG_MAX_NR + SOK_ENT_MAX_NR)

#define FREEMAP_START    0
#define FREEMAP_END      (FREEMAP_START + SOK_SEG_MAX_NR)
#define EIDMAP_START     FREEMAP_END
#define EIDMAP_END       (EIDMAP_START + SOK_ENT_MAX_NR)

static struct kmem_cache *vnode_oc;
static struct kmem_cache *inode_oc, *inode_pre_buf_oc;
static struct flatfs_oc  *dnode_oc;

struct fence_oc_desc {
    const char *name;
    size_t size;
    struct flatfs_oc *oc;
};

static struct fence_oc_desc fence_oc_descs[] = {
    { "fence-24",   24,   NULL },
    { "fence-40",   40,   NULL },
    { "fence-56",   56,   NULL },
    { "fence-72",   72,   NULL },
    { "fence-128",  128,  NULL },
    { "fence-200",  200,  NULL },
    { "fence-328",  328,  NULL },
    { "fence-528",  528,  NULL },
    { "fence-1024", 1024, NULL },
    { "fence-2048", 2048, NULL },
    { "fence-4096", 4096, NULL },
    { "fence-8192", 8192, NULL },
};

static inline void init_vnode_ocs(void) {
    vnode_oc = kmem_cache_create("flatfs-vnode", sizeof(brt_vnode_t), 0, SLAB_PANIC, NULL);
}

static inline void init_inode_ocs(void) {
    inode_oc = kmem_cache_create("flatfs-inode", sizeof(brt_inode_t), 256, SLAB_PANIC, NULL);
    inode_pre_buf_oc = kmem_cache_create("flatfs-inode-pre-buf", BRT_ND_PRE_BUF_SIZE, 256, SLAB_PANIC, NULL);
}

static inline void init_dnode_ocs(struct super_block *sb) {
    dnode_oc = flatfs_oc_create(sb, "flatfs-dnode", sizeof(brt_dnode_t) + BRT_ND_PRE_BUF_SIZE, 256);
}

static void init_fence_ocs(struct super_block *sb) {
    struct fence_oc_desc *desc;
    int i;
    for (i = 0; i < ARRAY_SIZE(fence_oc_descs); i++) {
        desc = &fence_oc_descs[i];
        desc->oc = flatfs_oc_create(sb, desc->name, desc->size, 0);
    }
}

static inline struct fence_oc_desc *get_fence_oc_desc(size_t size) {
    int i;
    for (i = 0; i < ARRAY_SIZE(fence_oc_descs); i++) {
        if (fence_oc_descs[i].size >= size) {
            return &fence_oc_descs[i];
        }
    }
    BUG();
}

void brt_node_init(struct super_block *sb) {
    init_vnode_ocs();
    init_inode_ocs();
    init_dnode_ocs(sb);
    init_fence_ocs(sb);
}

static inline void flush_buffer_if_leaf(sok_pool_t *pool, void *buf, uint32_t len, bool fence) {
    if (brt_node_is_leaf(pool)) {
        flatfs_flush_buffer(buf, len, fence);
    }
}

/*
 * INODE:
 * 11
 * 6  child           5B
 * 4  path_len        2B
 * 2  path_suf_cached 2B
 * 0  perm_off        2B
 *
 * DNODE:
 * 10
 * 6  ino             4B
 * 4  path_len        2B
 * 2  path_suf_cached 2B
 * 0  perm_off        2B
 */

static inline size_t path_start_off(sok_pool_t *pool) {
    return brt_node_is_leaf(pool) ? 10 : 12;
}

static inline size_t path_delta_len(sok_pool_t *pool) {
    return brt_node_is_leaf(pool) ? BRT_DNODE(pool->node)->delta_len : BRT_INODE(pool->node)->delta_len;
}

static inline u16 *path_len_p(sok_pool_t *pool, sok_entry_t ent) {
    return sok_get_ptr(pool, ent, 6);
}

static inline u16 *path_suf_cached_p(sok_pool_t *pool, sok_entry_t ent) {
    return sok_get_ptr(pool, ent, 4);
}

static inline u16 *perm_off_p(sok_pool_t *pool, sok_entry_t ent) {
    return sok_get_ptr(pool, ent, 2);
}

static inline void entry_to_slot(u8 *slot, sok_entry_t ent, int nr_seg) {
    int i;
    for (i = 0; i < nr_seg; i++) {
        slot[i] = ent.ids[i];
    }
}

static inline fastr_t path_pre(sok_pool_t *pool) {
    brt_node_t *node = pool->node;
    size_t len;
    void *buf;
    if (brt_node_is_leaf(pool)) {
        buf = BRT_DNODE(node)->pre_buf;
        len = BRT_DNODE(node)->vnode->pre_len;
    } else {
        buf = (void *) (FLATFS_VADDR_PRE + BRT_INODE(node)->pre_buf * 256ul);
        len = BRT_INODE(node)->pre_len;
    }
    return fastr(buf, len);
}

static inline void set_path_pre_len(sok_pool_t *pool, size_t len) {
    brt_node_t *node = pool->node;
    if (brt_node_is_leaf(pool)) {
        BRT_DNODE(node)->vnode->pre_len = len;
    } else {
        BRT_INODE(node)->pre_len = len;
    }
}

static inline size_t path_suf_len(sok_pool_t *pool, sok_entry_t ent) {
    fastr_t pre = path_pre(pool);
    return *path_len_p(pool, ent) - pre.len + path_delta_len(pool);
}

static inline fastr_t path_suf_get(sok_pool_t *pool, sok_entry_t ent) {
    size_t suf_len = path_suf_len(pool, ent);
    void *buf = kmalloc(suf_len, GFP_ATOMIC);
    sok_memcpy_from(pool, buf, ent, path_start_off(pool), suf_len);
    return fastr(buf, suf_len);
}

static inline int path_suf_cmp(sok_pool_t *pool, fastr_t str, sok_entry_t ent) {
    size_t suf_len = path_suf_len(pool, ent);
    size_t off = path_start_off(pool);
    return sok_strcmp(pool, str, ent, &off, suf_len);
}

static inline void path_suf_put(fastr_t suf) {
    kfree(suf.chars);
}

static inline u8 get_fgprt(fastr_t key) {
    return full_name_hash(NULL, key.chars, key.len);
}

static inline u8 get_fgprt_presuf(fastr_t pre, fastr_t suf) {
    /* TODO: This is only a workaround. */
    return get_fgprt(suf);
}

fastr_t brt_node_get_prefix(sok_pool_t *pool) {
    return path_pre(pool);
}

fastr_t brt_node_get_suffix(sok_pool_t *pool, sok_entry_t ent) {
    return path_suf_get(pool, ent);
}

void brt_node_get_partial_suffix(sok_pool_t *pool, void *buf, sok_entry_t ent, size_t len) {
    sok_memcpy_from(pool, buf, ent, path_start_off(pool), len);
}

size_t brt_node_suffix_find_first(sok_pool_t *pool, sok_entry_t haystack, char needle) {
    size_t off = path_start_off(pool);
    return sok_find_first(pool, haystack, off, path_suf_len(pool, haystack), needle) - off;
}

size_t brt_node_get_suffix_len(sok_pool_t *pool, sok_entry_t ent) {
    return path_suf_len(pool, ent);
}

void brt_node_put_suffix(sok_pool_t *pool, fastr_t suf) {
    path_suf_put(suf);
}

int brt_node_compare_suffix(sok_pool_t *pool, fastr_t str, sok_entry_t ent) {
    return path_suf_cmp(pool, str, ent);
}

size_t brt_node_match_suffix(sok_pool_t *pool, fastr_t str, sok_entry_t ent) {
    size_t off = path_start_off(pool), start;
    size_t suf_len = path_suf_len(pool, ent);
    start = off;
    sok_strcmp(pool, str, ent, &off, suf_len);
    return off - start;
}

/*
 * Find the first entry which is greater or equal than @key.
 * Return its order.
 * If the ent's key is not strictly equal to @key, then
 * @is_not_equal will be set.
 */
int brt_node_lowerbound(sok_pool_t *pool, fastr_t key, int *is_not_equal) {
    int l = 0, r = brt_node_nr_entries(pool);
    int neq = *is_not_equal = -1, cmp;
    fastr_t prefix = path_pre(pool);
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
        sok_entry_t ent = brt_node_get_entry_by_order(pool, mid);

        cmp = path_suf_cmp(pool, key, ent);
        if (cmp > 0) {
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

#define findless(x, n)       ((~0UL/255*(127+(n))-((x)&~0UL/255*127))&~(x)&~0UL/255*128)

__attribute__((target("bmi2")))
static inline u8 get_zeromask(unsigned long word, const struct word_at_a_time *constants) {
    return __builtin_ia32_pext_di(findless(word, 1), constants->high_bits);
}

static void get_fgprt_cmp_mask(unsigned long *mask, u8 *fgprts, u8 fgprt) {
	const struct word_at_a_time constants = WORD_AT_A_TIME_CONSTANTS;
    unsigned long r = REPEAT_BYTE(fgprt), w;
    int i;
    for (i = 0; i < DIV_ROUND_UP(BRT_MAX_NR_ENT, sizeof(unsigned long)); i++) {
        w = load_unaligned_zeropad(fgprts);
        ((u8 *) mask)[i] = get_zeromask(w ^ r, &constants);
        fgprts += sizeof(unsigned long);
    }
}

/*
 * Find entry with key @key.
 * Return its handle, or -ENOENT.
 */
int brt_node_find(sok_pool_t *pool, fastr_t key) {
    brt_dnode_t *dnode = BRT_DNODE(pool->node);
    fastr_t pre = path_pre(pool);
    u8 fgprt = get_fgprt(key);
    unsigned long mask;
    int eh;

    BUG_ON(!brt_node_is_leaf(pool));

    fastr_eliminate(&key, &pre);
    if (unlikely(pre.len)) {
        goto out;
    }

    get_fgprt_cmp_mask(&mask, dnode->fgprts, fgprt);
    bitmap_and(&mask, &mask, pool->eidmap, BRT_MAX_NR_ENT);

    for_each_set_bit(eh, &mask, BRT_MAX_NR_ENT) {
        sok_entry_t ent = brt_node_get_entry(pool, eh);
        if (likely(!path_suf_cmp(pool, key, ent))) {
            return eh;
        }
    }

out:
    return -ENOENT;
}

static inline size_t tri_eliminate(fastr_t *x, fastr_t *p, fastr_t *q) {
    size_t len = fastr_eliminate(x, p).len;
    if (fastr_is_empty(*p)) {
        len += fastr_eliminate(x, q).len;
    }
    return len;
}

static inline size_t unused_suf_start(sok_pool_t *pool, sok_entry_t ent) {
    return path_start_off(pool) + *path_suf_cached_p(pool, ent);
}

static inline fastr_t expand_prefetch(sok_pool_t *pool, sok_entry_t ent, fastr_t addon, size_t max_prefetch) {
    size_t final_len = addon.len, avail_len;

    final_len = max(final_len, BRT_PREFETCH_SIZE);

    avail_len = sok_sizeof(pool, ent) - unused_suf_start(pool, ent);
    final_len = min3(final_len, avail_len, max_prefetch + addon.len);

    addon.chars -= final_len - addon.len;
    addon.len = final_len;
    return addon;
}

static void ent_suffix_cache_expand(sok_pool_t *pool, sok_entry_t ent, fastr_t addon, size_t max_prefetch) {
    u16 *cached_len_p = path_suf_cached_p(pool, ent), new_len;
    size_t start = unused_suf_start(pool, ent);

    sok_resize(pool, ent, start + addon.len);
    addon = expand_prefetch(pool, ent, addon, max_prefetch);
    sok_memcpy_to(pool, ent, start, addon.chars, addon.len);
    sok_flush_range(pool, ent, start, addon.len);

    /* Newly cached entry data must be durable before changing cached length. */
    sok_persistent_barrier(pool);

    new_len = *cached_len_p + addon.len;
    flatfs_memcpy_atomic(cached_len_p, &new_len, sizeof(u16));
    flush_buffer_if_leaf(pool, cached_len_p, sizeof(u16), false);
}

static void ent_suffix_expand(sok_pool_t *pool, sok_entry_t ent, fastr_t addon, size_t max_prefetch) {
    size_t cur_len = path_suf_len(pool, ent), cached_len = *path_suf_cached_p(pool, ent);
    size_t new_len = cur_len + addon.len;

    if (unlikely(new_len > cached_len)) {
        ent_suffix_cache_expand(pool, ent, fastr_slice_before(addon, new_len - cached_len), max_prefetch);
    }

    /* Do nothing otherwise. Changing @pre_len will reflect all changes in suffix length. */
}

static void suffix_expand(sok_pool_t *pool, fastr_t addon, size_t max_prefetch) {
    DECLARE_BITMAP(ehmap, BRT_MAX_NR_ENT);
    int eh;
    brt_node_get_ehs(pool, ehmap);
    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        ent_suffix_expand(pool, brt_node_get_entry(pool, eh), addon, max_prefetch);
    }
}

static inline size_t get_filename_len(fastr_t pre, fastr_t suf) {
    size_t pos = fastr_find_last(suf, CTLCHR_COMPONENT_SEPARATOR);
    if (pos != FASTR_NPOS) {
        return suf.len - pos - 1;
    }
    return suf.len + pre.len - fastr_find_last(pre, CTLCHR_COMPONENT_SEPARATOR) - 1;
}

static inline sok_entry_t alloc_entry(sok_pool_t *pool, int *eid, fastr_t s0, fastr_t s1,
                                      size_t path_len, size_t filename_len) {
    size_t size, off;
    sok_entry_t ent;
    off = path_start_off(pool);
    size = max(s0.len + s1.len, filename_len) + off;
    ent = sok_alloc(pool, eid, size, filename_len + off);
    sok_memcpy_to(pool, ent, off, s1.chars, s1.len);
    sok_memcpy_to(pool, ent, off + s1.len, s0.chars, s0.len);
    *path_len_p(pool, ent) = path_len;
    *path_suf_cached_p(pool, ent) = s0.len + s1.len;
    return ent;
}

static int put(sok_pool_t *pool, int order,
               fastr_t s0, fastr_t s1, u8 fgprt, size_t path_len, size_t filename_len, unsigned long val) {
    int pos, is_not_equal, nr_entries = brt_node_nr_entries(pool), eid;
    brt_node_t *node = pool->node;
    sok_entry_t ent;

    /* Find where to insert. */
    if (!brt_node_is_leaf(pool)) {
        if (order == -1) {
            BUG_ON(!fastr_is_empty(s0));
            pos = brt_node_lowerbound(pool, s1, &is_not_equal);
        } else {
            pos = order;
            is_not_equal = 1;
        }
    } else {
        BUG_ON(!fastr_is_empty(s0));
        pos = brt_node_find(pool, s1);
        is_not_equal = pos != -ENOENT ? 0 : 1;
    }

    /* Update if exists. */
    if (unlikely(!is_not_equal)) {
        ent = brt_node_get_entry(pool, pos);
        goto update_val;
    }

    /* Write the entry. */
    ent = alloc_entry(pool, &eid, s0, s1, path_len, filename_len);
    if (pos == -ENOENT) {
        pos = eid;
    }

    /* Update the slot array (and fingerprint if necessary). */
    if (brt_node_is_leaf(pool)) {
        brt_dnode_t *dnode = BRT_DNODE(node);
        BUG_ON(!fastr_is_empty(s0));
        entry_to_slot(dnode->slots[pos], ent, BRT_DENT_NR_SEG);
        dnode->fgprts[pos] = fgprt;
    } else {
        brt_inode_t *inode = BRT_INODE(node);
        memmove(&inode->slots[pos + 1], &inode->slots[pos], nr_entries - pos);
        entry_to_slot(&inode->slots[pos], ent, BRT_IENT_NR_SEG);
        inode->nr_entries++;
    }

    /* Update value. */
update_val:
    brt_node_set_val(pool, ent, val);

    sok_flush_entry(pool, ent);

    return pos;
}

/*
 * Insert entry <@pre+@suf, @val> to @node.
 * If you know the order of key @pre+@suf in @node, you can pass it to @order
 * to eliminate the lowerbound query, otherwise pass -1.
 * Return the entry handle.
 */
int brt_node_insert(sok_pool_t *pool, int order, fastr_t pre, fastr_t suf, unsigned long val) {
    size_t match_len, path_len = pre.len + suf.len, filename_len = get_filename_len(pre, suf);
    fastr_t nd_path_pre = path_pre(pool), pre_mismatch;
    int nr_entries = brt_node_nr_entries(pool);
    u8 fgprt = get_fgprt_presuf(pre, suf);

    /*
     * When inserting into an empty node, the node's "prefix"
     * has not been well-defined. So we simply initialize the
     * prefix.
     */
    if (unlikely(nr_entries == 0)) {
        size_t old_len = nd_path_pre.len;
        fastr_append(&nd_path_pre, pre);
        fastr_append(&nd_path_pre, suf);
        flush_buffer_if_leaf(pool, nd_path_pre.chars, nd_path_pre.len - old_len, false);
        set_path_pre_len(pool, nd_path_pre.len);
    }

    /* Expand node suffixes if necessary. */
    pre_mismatch = nd_path_pre;
    match_len = tri_eliminate(&pre_mismatch, &pre, &suf);
    if (unlikely(!fastr_is_empty(pre_mismatch))) {
        suffix_expand(pool, pre_mismatch, match_len);
        set_path_pre_len(pool, nd_path_pre.len - pre_mismatch.len);
    }

    /* Put the new KV pair. */
    return put(pool, order, pre, suf, fgprt, path_len, filename_len, val);
}

static void compact(sok_pool_t *pool) {
    sok_seg_change_t change_list[SOK_SEG_MAX_NR];
    brt_dnode_t *dnode = BRT_DNODE(pool->node);
    int nr_change, i;
    u8 *slot;
    BUG_ON(!brt_node_is_leaf(pool));
    nr_change = sok_compact(pool, change_list);
    for (i = 0; i < nr_change; i++) {
        for (slot = (u8 *) dnode->slots; slot != (u8 *) dnode->slots + sizeof(dnode->slots); slot++) {
            if (*slot == change_list[i].from_id) {
                *slot = change_list[i].to_id;
            }
        }
    }
}

/*
 * Remove entry with handle @eh.
 */
void brt_node_remove(sok_pool_t *pool, int eh) {
    sok_entry_t ent = brt_node_get_entry(pool, eh);
    int nr_entries = brt_node_nr_entries(pool);
    brt_node_t *node = pool->node;

    /* Deallocate entry. */
    sok_dealloc(pool, ent);

    /* Remove from slot array. */
    if (!brt_node_is_leaf(pool)) {
        brt_inode_t *inode = BRT_INODE(node);
        memmove(&inode->slots[eh], &inode->slots[eh + 1], nr_entries - eh - 1);
        inode->nr_entries--;
    }
}

/*
 * Do compaction for dnodes.
 */
void brt_node_compact(sok_pool_t *pool) {
    compact(pool);
}

void brt_node_get_pool(sok_pool_t *pool, brt_tree_t *tree, brt_node_t *node) {
    int durable = nvm_ptr(tree->sbi, node);
    void *seg_arr = durable ? BRT_DNODE(node)->segs : BRT_INODE(node)->segs;
    int max_seg = durable ? BRT_DENT_NR_SEG : BRT_IENT_NR_SEG, i;
    DECLARE_BITMAP(freemap, SOK_SEG_MAX_NR) = { 0 };
    DECLARE_BITMAP(eidmap, SOK_ENT_MAX_NR) = { 0 };

    if (durable) {
        brt_dnode_t *dnode = BRT_DNODE(node);
        freemap[0] = dnode->bitmap[0];
        freemap[1] = dnode->bitmap[1] & ((1ul << (FREEMAP_END - BITS_PER_LONG)) - 1);
        eidmap[0] = dnode->bitmap[1] >> (EIDMAP_START - BITS_PER_LONG);
    } else {
        brt_inode_t *inode = BRT_INODE(node);
        for (i = 0; i < inode->nr_entries; i++) {
            bitmap_set(freemap, inode->slots[i], 1);
            bitmap_set(eidmap, i, 1);
        }
    }

    sok_pool_init(pool, tree->sbi->super, tree, node, seg_arr, max_seg, durable, freemap, eidmap);
}

fastr_t brt_node_get_lfence(brt_tree_t *tree, brt_node_t *node) {
    brt_dnode_t *dnode = BRT_DNODE(node);
    char *fences;
    BUG_ON(!__brt_node_is_leaf(tree, node));
    fences = flatfs_offset_to_address(tree->sbi->super, dnode->fences);
    return fastr(fences, dnode->lfence_len);
}

fastr_t brt_node_get_rfence(brt_tree_t *tree, brt_node_t *node) {
    brt_dnode_t *dnode = BRT_DNODE(node);
    char *fences;
    BUG_ON(!__brt_node_is_leaf(tree, node));
    fences = flatfs_offset_to_address(tree->sbi->super, dnode->fences);
    return fastr(fences + dnode->lfence_len, dnode->rfence_len);
}

static inline void perm_cache_init(struct brt_perm_cache *perm_cache) {
    perm_cache->perm_ver = 0;
    perm_cache->curr_ver = 1;
}

static brt_vnode_t *vnode_create(brt_tree_t *tree, brt_dnode_t *dnode) {
    brt_vnode_t *vnode = kmem_cache_alloc(vnode_oc, GFP_ATOMIC);
    rwlock_init(&vnode->lock);
    perm_cache_init(&vnode->perm_cache);
    INIT_LIST_HEAD(&vnode->leaf_list);
    vnode->dnode = dnode;
    vnode->lfence = brt_node_get_lfence(tree, dnode);
    vnode->rfence = brt_node_get_rfence(tree, dnode);
    vnode->pre_len = dnode->pre_len;
    vnode->refcount = 1;
    return vnode;
}

static void flush_metadata(sok_pool_t *pool) {
    if (brt_node_is_leaf(pool)) {
        brt_dnode_t *dnode = BRT_DNODE(pool->node);
        flatfs_flush_buffer(dnode, 3 * CACHELINE_SIZE, false);
    }
}

static inline void dnode_set_fence(struct super_block *sb, brt_dnode_t *dnode, fastr_t lfence, fastr_t rfence) {
    size_t size = lfence.len + rfence.len;
    struct flatfs_oc *oc = get_fence_oc_desc(size)->oc;
    char *buf = flatfs_oc_alloc(oc, GFP_ATOMIC);
    dnode->lfence_len = lfence.len;
    dnode->rfence_len = rfence.len;
    dnode->fences = flatfs_address_to_offset(sb, buf);
    memcpy(buf, lfence.chars, lfence.len);
    memcpy(buf + lfence.len, rfence.chars, rfence.len);
    flatfs_flush_buffer(buf, size, false);
}

/*
 * Create new data node.
 */
static brt_dnode_t *create_dnode(brt_tree_t *tree, fastr_t pre, fastr_t addon, u16 delta_len, u64 next,
                                 fastr_t lfence, fastr_t rfence) {
    brt_dnode_t *dnode = flatfs_oc_alloc(dnode_oc, GFP_ATOMIC);
    struct super_block *sb = tree->sbi->super;
    char *buf = dnode->pre_buf;
    fastr_t nd_pre = fastr(buf, 0);
    fastr_append(&nd_pre, pre);
    fastr_append(&nd_pre, addon);
    memset(dnode->bitmap, 0, sizeof(dnode->bitmap));
    dnode_set_fence(sb, dnode, lfence, rfence);
    dnode->pre_len = pre.len + addon.len;
    dnode->delta_len = delta_len;
    dnode->vnode = vnode_create(tree, dnode);
    dnode->next = next;
    return dnode;
}

/*
 * Create new index node
 */
static brt_inode_t *create_inode(int is_border, fastr_t pre, u16 delta_len, u64 min_child) {
    brt_inode_t *inode = kmem_cache_alloc(inode_oc, GFP_ATOMIC);
    char *buf = kmem_cache_alloc(inode_pre_buf_oc, GFP_ATOMIC);
    fastr_t nd_pre = fastr(buf, 0);
    fastr_append(&nd_pre, pre);
    rwlock_init(&inode->lock);
    inode->pre_len = pre.len;
    inode->delta_len = delta_len;
    inode->is_border = is_border;
    inode->nr_entries = 0;
    inode->pre_buf = ((u64) buf - FLATFS_VADDR_PRE) / 256;
    inode->min_child = min_child;
    return inode;
}

static inline void bitmap_shift_left_or(unsigned long *dst, const unsigned long *src, int shift) {
    DECLARE_BITMAP(tmp, BITMAP_LEN);
    if (shift) {
        bitmap_shift_left(tmp, src, shift, BITMAP_LEN);
    }
    bitmap_or(dst, dst, tmp, BITMAP_LEN);
}

static inline void sync_bitmap(unsigned long *bitmap, sok_pool_t *pool) {
    bitmap[0] = pool->freemap[0];
    bitmap[1] = pool->freemap[1] | (pool->eidmap[0] << (EIDMAP_START - BITS_PER_LONG));
}

/*
 * Copy entries specified in @ehmap from @src to @dst.
 */
void brt_node_copy(sok_pool_t *dst, sok_pool_t *src, unsigned long *ehmap, fastr_t addon,
                   brt_node_t *min_child_or_next, fastr_t lfence, fastr_t rfence) {
    fastr_t pre = brt_node_get_prefix(src);
    sok_entry_t ent, ents[BRT_MAX_NR_ENT];
    int eh, eid, nr_ents = 0;
    brt_node_t *node;

    if (brt_node_is_leaf(src)) {
        __le64 off = min_child_or_next ? flatfs_address_to_offset(src->sb, min_child_or_next) : BRT_NOFF;
        node = create_dnode(src->tree, pre, addon, 0, off, lfence, rfence);
    } else {
        node = create_inode(BRT_INODE(src->node)->is_border, pre, 0, brt_node_child_ptr2off(src, min_child_or_next));
    }

    brt_node_get_pool(dst, src->tree, node);

    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        ent = brt_node_get_entry(src, eh);
        ents[nr_ents] = sok_copy(dst, &eid, src, ent);

        if (brt_node_is_leaf(dst)) {
            brt_dnode_t *dsrc = BRT_DNODE(src->node);
            brt_dnode_t *ddst = BRT_DNODE(dst->node);
            ddst->fgprts[eid] = dsrc->fgprts[eh];
            entry_to_slot(ddst->slots[eid], ents[nr_ents], BRT_DENT_NR_SEG);
        } else {
            brt_inode_t *inode = BRT_INODE(dst->node);
            entry_to_slot(&inode->slots[nr_ents], ents[nr_ents], BRT_IENT_NR_SEG);
        }

        nr_ents++;
    }

    sok_flush_entries(dst, nr_ents, ents);

    if (brt_node_is_leaf(dst)) {
        sync_bitmap((ulong *) BRT_DNODE(dst->node)->bitmap, dst);
    } else {
        BRT_INODE(dst->node)->nr_entries = nr_ents;
    }

    flush_metadata(dst);

    sok_persistent_barrier(dst);
}

/*
 * Create tree root node.
 */
void brt_node_create_root(sok_pool_t *dst, brt_tree_t *tree) {
    struct super_block *sb = tree->sbi->super;
    brt_node_t *root;
    if (!tree->root) {
        root = create_dnode(tree, FASTR_NULL, FASTR_NULL, 0, BRT_NOFF, FASTR_NULL, FASTR_LITERAL("\x7f"));
        brt_node_get_pool(dst, tree, root);
    } else {
        root = create_inode(nvm_ptr(tree->sbi, tree->root), FASTR_NULL, 0, 0);
        brt_node_get_pool(dst, tree, root);
        BRT_INODE(dst->node)->min_child = brt_node_child_ptr2off(dst, tree->root);
    }
}

/*
 * Shift common prefixes of suffixes to node prefix.
 */
void brt_node_suf2pre(sok_pool_t *pool) {
    brt_inode_t *inode = BRT_INODE(pool->node);
    fastr_t suf_min, suf_max, addon, pre, p, q;
    int nr_ents = inode->nr_entries;

    BUG_ON(brt_node_is_leaf(pool));

    suf_min = brt_node_get_suffix(pool, brt_node_get_entry(pool, 0));
    suf_max = brt_node_get_suffix(pool, brt_node_get_entry(pool, nr_ents - 1));

    p = suf_min;
    q = suf_max;
    addon = fastr_eliminate(&p, &q);

    pre = brt_node_get_prefix(pool);
    fastr_append(&pre, addon);
    inode->pre_len = pre.len;

    brt_node_put_suffix(pool, suf_min);
    brt_node_put_suffix(pool, suf_max);
}

/*
 * Commit node update.
 */
void brt_node_commit(sok_pool_t *pool) {
    brt_dnode_t *dnode = BRT_DNODE(pool->node);
    unsigned long bitmap[2] = { 0 };
    brt_vnode_t *vnode;
    size_t pre_len;

    BUG_ON(!brt_node_is_leaf(pool));

    brt_node_barrier(pool);

    vnode = dnode->vnode;

    pre_len = vnode->pre_len;
    if (dnode->pre_len != pre_len) {
        flatfs_memcpy_atomic(&dnode->pre_len, &pre_len, sizeof(dnode->pre_len));
        flatfs_flush_buffer(&dnode->pre_len, sizeof(dnode->pre_len), true);
    }

    sync_bitmap(bitmap, pool);

    cmpxchg_double_local(&dnode->bitmap[0], &dnode->bitmap[1],
                         dnode->bitmap[0], dnode->bitmap[1],
                         bitmap[0], bitmap[1]);
    flatfs_flush_buffer(&dnode->bitmap, sizeof(dnode->bitmap), true);

    sok_commit(pool);

    dnode->vnode->perm_cache.curr_ver++;
}

void brt_node_dump_entries(sok_pool_t *pool) {
    DECLARE_BITMAP(ehmap, BRT_MAX_NR_ENT);
    int eh;

    brt_node_get_ehs(pool, ehmap);

    printk("node %p:\n", pool->node);
    printk("prefix: %.*s\n", FASTR_FMT(brt_node_get_prefix(pool)));

    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        fastr_t suf = brt_node_get_suffix(pool, brt_node_get_entry(pool, eh));
        printk("eh: %d suffix: %.*s\n", eh, FASTR_FMT(suf));
        brt_node_put_suffix(pool, suf);
    }
}

static inline void dnode_free(brt_node_t *node) {
    brt_dnode_t *dnode = BRT_DNODE(node);
    flatfs_oc_free(dnode_oc, dnode);
}

static void __dnode_free(struct rcu_head *rcu) {
    brt_dnode_t *dnode = container_of(rcu, brt_vnode_t, rcu)->dnode;
    dnode_free(dnode);
}

void brt_dnode_hold(brt_tree_t *tree, brt_node_t *node) {
    brt_dnode_t *dnode = BRT_DNODE(node);
    BUG_ON(!__brt_node_is_leaf(tree, node));
    xadd(&dnode->vnode->refcount, 1);
}

void brt_dnode_put(brt_tree_t *tree, brt_node_t *node) {
    brt_dnode_t *dnode = BRT_DNODE(node);
    BUG_ON(!__brt_node_is_leaf(tree, node));
    if (xadd(&dnode->vnode->refcount, -1) == 1) {
        call_rcu(&dnode->vnode->rcu, __dnode_free);
    }
}

static inline void inode_free(sok_pool_t *pool) {
    brt_inode_t *inode = BRT_INODE(pool->node);
    BUG_ON(brt_node_is_leaf(pool));
    kmem_cache_free(inode_pre_buf_oc, path_pre(pool).chars);
    kmem_cache_free(inode_oc, inode);
}

void brt_node_put(sok_pool_t *pool) {
    if (brt_node_is_leaf(pool)) {
        brt_dnode_put(pool->tree, pool->node);
    } else {
        inode_free(pool);
    }
}

void brt_node_invalidate(sok_pool_t *pool) {
    struct flatfs_oc *fence_oc;
    brt_dnode_t *dnode;
    BUG_ON(!brt_node_is_leaf(pool));
    dnode = BRT_DNODE(pool->node);
    fence_oc = get_fence_oc_desc(dnode->lfence_len + dnode->rfence_len)->oc;
    dnode->vnode->lfence = dnode->vnode->rfence = FASTR_NULL;
    flatfs_oc_free(fence_oc, flatfs_offset_to_address(pool->sb, dnode->fences));
}

void brt_node_prealloc(struct super_block *sb, int nr) {
    int i;
    for (i = 0; i < ARRAY_SIZE(fence_oc_descs); i++) {
        /* Each node has at most one fence. */
        flatfs_oc_reserve(fence_oc_descs[i].oc, nr, 0);
    }
    flatfs_oc_reserve(dnode_oc, nr, 0);
}

void brt_node_preinsert(struct super_block *sb, int nr) {
    sok_prealloc(sb, BRT_MAX_NR_ENT);
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

static void sync_perm_arr(sok_pool_t *pool) {
    brt_dnode_t *dnode = BRT_DNODE(pool->node);
    struct brt_perm_cache *perm_cache = &dnode->vnode->perm_cache;
    struct dnode_key keys[BRT_MAX_NR_ENT];
    DECLARE_BITMAP(ehmap, BRT_MAX_NR_ENT);
    int eh, nr_ents = 0, i;

    if (likely(perm_cache->perm_ver == perm_cache->curr_ver)) {
        return;
    }

    sok_prefetch(pool);

    brt_node_get_ehs(pool, ehmap);
    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        keys[nr_ents].suf = brt_node_get_suffix(pool, brt_node_get_entry(pool, eh));
        keys[nr_ents].eh = eh;
        nr_ents++;
    }

    _quicksort(keys, nr_ents);

    for (i = 0; i < nr_ents; i++) {
        perm_cache->perm_arr[i] = keys[i].eh;
        brt_node_put_suffix(pool, keys[i].suf);
    }

    barrier();

    perm_cache->perm_ver = perm_cache->curr_ver;
}

static inline void get_perm_arr(u8 *perm_arr, sok_pool_t *pool) {
    brt_dnode_t *dnode = BRT_DNODE(pool->node);
    struct brt_perm_cache *perm_cache = &dnode->vnode->perm_cache;
    sync_perm_arr(pool);
    memcpy(perm_arr, perm_cache->perm_arr, sizeof(perm_cache->perm_arr));
}

int brt_node_order_to_eh(sok_pool_t *pool, int order) {
    if (!brt_node_is_leaf(pool)) {
        return order;
    } else {
        u8 perm_arr[BRT_MAX_NR_ENT];
        get_perm_arr(perm_arr, pool);
        return perm_arr[order];
    }
}

brt_node_t *brt_node_get_and_lock_next(sok_pool_t *pool, bool w) {
    struct list_head *currl = &brt_vnode_get(pool->node)->leaf_list, *nextl;
    struct flatfs_sb_info *sbi = pool->tree->sbi;
    brt_dnode_t *next = NULL;

    rcu_read_lock();

reacquire:
    nextl = READ_ONCE(currl->next);
    if (unlikely(nextl == &sbi->leaf_list_head)) {
        goto out;
    }

    next = list_entry(nextl, brt_vnode_t, leaf_list)->dnode;

    brt_node_lock(pool->tree, next, w);
    if (unlikely(READ_ONCE(currl->next) != nextl)) {
        brt_node_unlock(pool->tree, next, w);
        goto reacquire;
    }

out:
    rcu_read_unlock();

    return next;
}

static inline void stat_range(int stat[SOK_ENT_MAX_SEG + 1][2],
                              sok_pool_t *pool, sok_entry_t ent, size_t start, size_t len) {
    int nr_segs, nr_off_pgs;
    sok_entry_stat(pool, &nr_off_pgs, &nr_segs, ent, start, len);
    stat[nr_segs][nr_off_pgs]++;
}

static inline void stat_filename(brt_ndstat_t *stat, sok_pool_t *pool, sok_entry_t ent) {
    size_t start = path_start_off(pool), len;
    len = sok_find_first(pool, ent, start, path_suf_len(pool, ent), CTLCHR_COMPONENT_SEPARATOR) - start;
    stat_range(stat->nr_filename, pool, ent, start, len);
    stat->total_filename_len += len;
}

static inline void stat_suf(brt_ndstat_t *stat, sok_pool_t *pool, sok_entry_t ent) {
    size_t start = path_start_off(pool), len = path_suf_len(pool, ent);
    stat_range(stat->nr_suf, pool, ent, start, len);
    stat->total_suf_len += len;
}

static inline void stat_suf_cached(brt_ndstat_t *stat, sok_pool_t *pool, sok_entry_t ent) {
    size_t start = path_start_off(pool), len = *path_suf_cached_p(pool, ent);
    stat_range(stat->nr_suf_cached, pool, ent, start, len);
    stat->total_suf_cached_len += len;
}

static inline void stat_path(brt_ndstat_t *stat, sok_pool_t *pool, sok_entry_t ent) {
    stat->total_path_len += path_pre(pool).len + path_suf_len(pool, ent);
}

static void entry_stat(brt_ndstat_t *stat, sok_pool_t *pool, sok_entry_t ent) {
    stat_filename(stat, pool, ent);
    stat_suf(stat, pool, ent);
    stat_suf_cached(stat, pool, ent);
    stat_path(stat, pool, ent);
}

void brt_node_stat(brt_ndstat_t *stat, sok_pool_t *pool) {
    DECLARE_BITMAP(ehmap, BRT_MAX_NR_ENT);
    int eh;
    memset(stat, 0, sizeof(*stat));
    stat->nr_entries = brt_node_nr_entries(pool);
    brt_node_get_ehs(pool, ehmap);
    for_each_set_bit(eh, ehmap, BRT_MAX_NR_ENT) {
        sok_entry_t ent = brt_node_get_entry(pool, eh);
        entry_stat(stat, pool, ent);
    }
}
