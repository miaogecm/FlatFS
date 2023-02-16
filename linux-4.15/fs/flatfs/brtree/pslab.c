/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Scalable and persistent object-caching slab allocator
 *
 * Architecture:
 *      Scalable CPU Cache Manager: Manage per-CPU object cache
 *      Global Cache Manager: Manage global object cache
 *      PSlab Manager: Map pslab to objects
 *      Backend Block Supplier: FlatFS block allocator
 */

#include <linux/kernel.h>

#include "../flatfs.h"

#define OC_NAME_MAX_LEN         31

#define MIN_NR_OBJ_PER_PSLAB    8

#define PSLAB_HDR_MAGIC         0xdeaddeadbeefbeef

struct cpu_cache {
    __le64 freelist;
    __le64 nr_free;

    /* volatile */
    long nr_alloc_action;
    long nr_free_action;
} ____cacheline_aligned;

struct global_cache {
    spinlock_t *lock;
    __le64 freelist;
    __le64 nr_free;
};

struct pslab_hdr {
    __le64 magic;
    __le64 oc;
    __le64 inuse;
} ____cacheline_aligned;

struct flatfs_oc {
    struct cpu_cache cpu_caches[FLATFS_NCPU];
    struct global_cache global_cache;

    __le32 pslab_page_type;
    __le64 obj_size, nr_chunk_per_pslab, data_off;
    __le64 next;

    __le64 nr_obj_move_global_to_cpu;
    __le64 cpu_cache_gc_threshold, global_cache_gc_threshold;

    char name[OC_NAME_MAX_LEN + 1];

    /* volatile */
    struct super_block *sb;
    long nr_balloc_action;
    long nr_bfree_action;
};

struct flatfs_oc_desc {
    struct flatfs_oc meta_oc;

    spinlock_t *lock;
    __le64 head;
};

static inline const char *page_name(int page_type) {
    switch (page_type) {
        case FLATFS_BLOCK_TYPE_4K:
            return "4K";
        case FLATFS_BLOCK_TYPE_2M:
            return "2M";
        case FLATFS_BLOCK_TYPE_1G:
            return "1G";
        default:
            BUG();
    }
}

static inline int page_shift(int page_type) {
    switch (page_type) {
        case FLATFS_BLOCK_TYPE_4K:
            return PAGE_SHIFT;
        case FLATFS_BLOCK_TYPE_2M:
            return PAGE_SHIFT_2M;
        case FLATFS_BLOCK_TYPE_1G:
            return PAGE_SHIFT_1G;
        default:
            BUG();
    }
}

static inline size_t page_size(int page_type) {
    switch (page_type) {
        case FLATFS_BLOCK_TYPE_4K:
            return PAGE_SIZE_4K;
        case FLATFS_BLOCK_TYPE_2M:
            return PAGE_SIZE_2M;
        case FLATFS_BLOCK_TYPE_1G:
            return PAGE_SIZE_1G;
        default:
            BUG();
    }
}

static inline struct pslab_hdr *pslab_hdr_of(struct flatfs_oc *oc, void *obj) {
    size_t size = page_size((int) oc->pslab_page_type);
    return (struct pslab_hdr *) ((unsigned long) obj & ~(size - 1));
}

static inline long get_nr_obj_move_global_to_cpu(struct flatfs_oc *oc) {
    return (long) oc->nr_obj_move_global_to_cpu;
}

static inline long get_cpu_cache_gc_threshold(struct flatfs_oc *oc) {
    return (long) oc->cpu_cache_gc_threshold;
}

static inline long get_global_cache_gc_threshold(struct flatfs_oc *oc) {
    return (long) oc->global_cache_gc_threshold;
}

static inline long get_nr_cpu_cache_gc_obj(struct flatfs_oc *oc, struct cpu_cache *cpu_cache) {
    return (long) cpu_cache->nr_free / 2;
}

static inline long get_nr_global_cache_gc_obj(struct flatfs_oc *oc, struct global_cache *global_cache) {
    return (long) global_cache->nr_free / 2;
}

static inline struct cpu_cache *get_cpu_cache(struct flatfs_oc *oc) {
    int cpu = get_cpu();
    return &oc->cpu_caches[cpu];
}

static inline void put_cpu_cache(struct cpu_cache *cache) {
    put_cpu();
}

static long pslab_alloc(struct flatfs_oc *oc, __le64 *head, __le64 *tail, long nr, gfp_t flags) {
    long nr_pslab = DIV_ROUND_UP(nr, oc->nr_chunk_per_pslab), i, j;
    void *pslab, *data, *last = NULL;
    __le64 shead = 0, stail = 0;
    struct pslab_hdr *hdr;
    unsigned long blknr;
    int ret;

    /* @flatfs_new_block may sleep. Can not do that in atomic section. */
    BUG_ON(flags & GFP_ATOMIC);

    for (i = 0; i < nr_pslab; i++) {
        ret = flatfs_new_block(oc->sb, &blknr, oc->pslab_page_type, 0);
        BUG_ON(ret);
        pslab = flatfs_get_block(oc->sb, flatfs_get_block_off(oc->sb, blknr, oc->pslab_page_type));
        data = pslab + oc->data_off;
        hdr = pslab;
        hdr->magic = PSLAB_HDR_MAGIC;
        hdr->oc = flatfs_address_to_offset(oc->sb, oc);
        hdr->inuse = oc->nr_chunk_per_pslab;
        flatfs_flush_buffer(hdr, sizeof(*hdr), false);
        for (j = 0; j < oc->nr_chunk_per_pslab; j++) {
            stail = flatfs_address_to_offset(oc->sb, data);
            if (likely(last)) {
                *(__le64 *) last = stail;
                flatfs_flush_buffer(last, sizeof(__le64), false);
            } else {
                shead = stail;
            }
            last = data;
            data += oc->obj_size;
        }
        oc->nr_balloc_action++;
    }
    if (last) {
        *(__le64 *) last = 0;
        flatfs_flush_buffer(last, sizeof(__le64), false);
    }
    PERSISTENT_BARRIER();

    *head = shead;
    *tail = stail;

    return (long) oc->nr_chunk_per_pslab * nr_pslab;
}

static void pslab_free(struct flatfs_oc *oc, __le64 head, __le64 tail, long nr) {
    struct pslab_hdr *hdr;
    unsigned long blknr;
    __le64 new_inuse;
    void *curr;
    long i;

    (void) tail;

    for (i = 0; i < nr; i++) {
        curr = flatfs_offset_to_address(oc->sb, head);
        hdr = pslab_hdr_of(oc, curr);
        new_inuse = hdr->inuse - 1;
        if (new_inuse) {
            flatfs_memcpy_atomic(&hdr->inuse, &new_inuse, sizeof(new_inuse));
            flatfs_flush_buffer(&hdr->inuse, sizeof(new_inuse), false);
        } else {
            blknr = flatfs_get_blocknr(oc->sb, flatfs_address_to_offset(oc->sb, hdr), oc->pslab_page_type);
            flatfs_free_block(oc->sb, blknr, oc->pslab_page_type);
            oc->nr_bfree_action++;
        }
        head = *(__le64 *) curr;
    }
    PERSISTENT_BARRIER();
}

static void global_gc(struct flatfs_oc *oc, struct global_cache *global_cache) {
    long nr_gc_obj = get_nr_global_cache_gc_obj(oc, global_cache), i;
    __le64 off = global_cache->freelist, head, tail;
    void *curr;

    for (i = 0; i < nr_gc_obj; i++) {
        curr = flatfs_offset_to_address(oc->sb, off);
        off = *(__le64 *) curr;
    }

    head = global_cache->freelist;
    tail = flatfs_address_to_offset(oc->sb, curr);

    global_cache->nr_free -= nr_gc_obj;

    flatfs_memcpy_atomic(&global_cache->freelist, &off, sizeof(off));
    flatfs_flush_buffer(&global_cache->freelist, sizeof(off), true);

    /* @pslab_free might sleep. Exit the atomic section. */
    spin_unlock(global_cache->lock);

    pslab_free(oc, head, tail, nr_gc_obj);

    FLATFS_INFO(PSLAB_GC, "%s: GC @ Global Cache, return %ld objects from global cache to pslab, %llu remains",
                oc->name, nr_gc_obj, global_cache->nr_free);

    spin_lock(global_cache->lock);
}

static void global_alloc(struct flatfs_oc *oc, __le64 *head, __le64 *tail, long nr, gfp_t flags) {
    struct global_cache *global_cache = &oc->global_cache;
    long i, nr_transfer, nr_free;
    __le64 off, chead, ctail;
    void *curr, *chunk;

    spin_lock(global_cache->lock);

retry:
    nr_free = (long) global_cache->nr_free;

    if (unlikely(nr_free < nr)) {
        spin_unlock(global_cache->lock);

        nr_transfer = pslab_alloc(oc, &chead, &ctail, nr - nr_free, flags);

        spin_lock(global_cache->lock);

        chunk = flatfs_offset_to_address(oc->sb, ctail);
        flatfs_memcpy_atomic(chunk, &global_cache->freelist, sizeof(global_cache->freelist));
        flatfs_flush_buffer(chunk, sizeof(global_cache->freelist), true);

        flatfs_memcpy_atomic(&global_cache->freelist, &chead, sizeof(chead));
        flatfs_flush_buffer(&global_cache->freelist, sizeof(chead), true);

        global_cache->nr_free += nr_transfer;

        FLATFS_INFO(PSLAB_GROW, "%s: Grow @ Global Cache, move %ld objects from pslab to global cache, %llu remains",
                    oc->name, nr_transfer, global_cache->nr_free);

        goto retry;
    }

    off = *head = global_cache->freelist;
    for (i = 0; i < nr; i++) {
        curr = flatfs_offset_to_address(oc->sb, off);
        off = *(__le64 *) curr;
    }

    *tail = flatfs_address_to_offset(oc->sb, curr);

    global_cache->nr_free -= nr;

    flatfs_memcpy_atomic(&global_cache->freelist, &off, sizeof(off));
    flatfs_flush_buffer(&global_cache->freelist, sizeof(off), true);

    spin_unlock(global_cache->lock);
}

static void global_free(struct flatfs_oc *oc, __le64 head, __le64 tail, long nr) {
    struct global_cache *global_cache = &oc->global_cache;
    void *chunk = flatfs_offset_to_address(oc->sb, tail);

    spin_lock(global_cache->lock);

    /*
     * TODO: We temporarily disable the global GC as @global_gc may block. We need
     * some techniques like asynchronous/periodic pslab recycle. @flatfs_oc_free
     * should behave like @kmem_cache_free which never sleeps.
     */
#if 0
    /* GC to pslab if we have too many free chunks in the global cache. */
    if (unlikely(global_cache->nr_free >= get_global_cache_gc_threshold(oc))) {
        global_gc(oc, global_cache);
    }
#endif

    global_cache->nr_free += nr;

    flatfs_memcpy_atomic(chunk, &global_cache->freelist, sizeof(global_cache->freelist));
    flatfs_flush_buffer(chunk, sizeof(global_cache->freelist), true);

    flatfs_memcpy_atomic(&global_cache->freelist, &head, sizeof(head));
    flatfs_flush_buffer(&global_cache->freelist, sizeof(head), true);

    spin_unlock(global_cache->lock);
}

static void cpu_gc(struct flatfs_oc *oc, struct cpu_cache *cpu_cache) {
    long nr_gc_obj = get_nr_cpu_cache_gc_obj(oc, cpu_cache), i;
    __le64 off = cpu_cache->freelist, head, tail;
    void *curr;

    for (i = 0; i < nr_gc_obj; i++) {
        curr = flatfs_offset_to_address(oc->sb, off);
        off = *(__le64 *) curr;
    }

    head = cpu_cache->freelist;
    tail = flatfs_address_to_offset(oc->sb, curr);

    cpu_cache->nr_free -= nr_gc_obj;

    flatfs_memcpy_atomic(&cpu_cache->freelist, &off, sizeof(off));
    flatfs_flush_buffer(&cpu_cache->freelist, sizeof(off), true);

    global_free(oc, head, tail, nr_gc_obj);

    FLATFS_INFO(PSLAB_GC, "%s: GC @ CPU%u, move %ld objects from CPU cache to global cache, %llu remains",
                oc->name, smp_processor_id(), nr_gc_obj, cpu_cache->nr_free);
}

static void cpu_reserve(struct flatfs_oc *oc, long nr, gfp_t flags) {
    struct cpu_cache *cpu_cache = get_cpu_cache(oc);
    struct super_block *sb = oc->sb;
    __le64 head, tail;
    long nr_transfer;
    void *chunk;

    if (likely(cpu_cache->nr_free >= nr)) {
        return;
    }

    nr_transfer = max(get_nr_obj_move_global_to_cpu(oc), nr);
    global_alloc(oc, &head, &tail, nr_transfer, flags);

    cpu_cache->nr_free += nr_transfer;

    FLATFS_INFO(PSLAB_GROW, "%s: Grow @ CPU%u, move %ld objects from global cache to CPU cache, %llu remains",
                oc->name, smp_processor_id(), nr_transfer, cpu_cache->nr_free);

    chunk = flatfs_offset_to_address(sb, tail);
    flatfs_memcpy_atomic(chunk, &cpu_cache->freelist, sizeof(cpu_cache->freelist));
    flatfs_flush_buffer(chunk, sizeof(cpu_cache->freelist), true);

    flatfs_memcpy_atomic(&cpu_cache->freelist, &head, sizeof(head));
    flatfs_flush_buffer(&cpu_cache->freelist, sizeof(head), true);

    put_cpu_cache(cpu_cache);
}

static void *cpu_alloc(struct flatfs_oc *oc, gfp_t flags) {
    struct cpu_cache *cpu_cache = get_cpu_cache(oc);
    struct super_block *sb = oc->sb;
    __le64 head, tail, noff = 0;
    long nr_transfer;
    void *chunk;

    /* Ensure we have enough cpu cache. */
    if (unlikely(!cpu_cache->freelist)) {
        nr_transfer = get_nr_obj_move_global_to_cpu(oc);
        global_alloc(oc, &head, &tail, nr_transfer, flags);

        cpu_cache->nr_free += nr_transfer;

        FLATFS_INFO(PSLAB_GROW, "%s: Grow @ CPU%u, move %ld objects from global cache to CPU cache, %llu remains",
                    oc->name, smp_processor_id(), nr_transfer, cpu_cache->nr_free);

        chunk = flatfs_offset_to_address(sb, tail);
        flatfs_memcpy_atomic(chunk, &noff, sizeof(noff));
        flatfs_flush_buffer(chunk, sizeof(noff), true);
    } else {
        head = cpu_cache->freelist;
    }

    /* Allocate from CPU cache. */
    chunk = flatfs_offset_to_address(sb, head);
    head = *(__le64 *) chunk;

    cpu_cache->nr_free--;

    flatfs_memcpy_atomic(&cpu_cache->freelist, &head, sizeof(head));
    flatfs_flush_buffer(&cpu_cache->freelist, sizeof(head), true);

    cpu_cache->nr_alloc_action++;

    put_cpu_cache(cpu_cache);

    return chunk;
}

static inline void verify_free(struct flatfs_oc *oc, void *obj) {
    struct pslab_hdr *hdr = pslab_hdr_of(oc, obj);
    BUG_ON(hdr->magic != PSLAB_HDR_MAGIC);
    BUG_ON(oc != flatfs_offset_to_address(oc->sb, hdr->oc));
}

static void cpu_free(struct flatfs_oc *oc, void *obj) {
    struct cpu_cache *cpu_cache = get_cpu_cache(oc);
    struct super_block *sb = oc->sb;
    __le64 off = flatfs_address_to_offset(sb, obj);

    /* GC to global cache if we have too many free chunks in the CPU cache. */
    if (unlikely(cpu_cache->nr_free >= get_cpu_cache_gc_threshold(oc))) {
        cpu_gc(oc, cpu_cache);
    }

    /* Put it back to the CPU cache. */
    cpu_cache->nr_free++;

    flatfs_memcpy_atomic(obj, &cpu_cache->freelist, sizeof(off));
    flatfs_flush_buffer(obj, sizeof(off), true);

    flatfs_memcpy_atomic(&cpu_cache->freelist, &off, sizeof(off));
    flatfs_flush_buffer(&cpu_cache->freelist, sizeof(off), true);

    cpu_cache->nr_free_action++;

    put_cpu_cache(cpu_cache);
}

static inline struct flatfs_oc_desc *get_desc(struct super_block *sb) {
    struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
    struct flatfs_oc_desc *desc = flatfs_offset_to_address(sb, flatfs_sb->s_oc_desc);
    return desc;
}

static inline struct flatfs_oc *get_meta_oc(struct super_block *sb) {
    return &get_desc(sb)->meta_oc;
}

static int get_page_type(struct flatfs_oc *oc) {
    if (MIN_NR_OBJ_PER_PSLAB * oc->obj_size <= PAGE_SIZE_4K - oc->data_off) {
        return FLATFS_BLOCK_TYPE_4K;
    } else if (MIN_NR_OBJ_PER_PSLAB * oc->obj_size <= PAGE_SIZE_2M - oc->data_off) {
        return FLATFS_BLOCK_TYPE_2M;
    } else if (MIN_NR_OBJ_PER_PSLAB * oc->obj_size <= PAGE_SIZE_1G - oc->data_off) {
        return FLATFS_BLOCK_TYPE_1G;
    }
    BUG();
}

static void set_pslab_info(struct flatfs_oc *oc) {
    int type = get_page_type(oc);
    oc->pslab_page_type = type;
    oc->nr_chunk_per_pslab = (page_size(type) - oc->data_off) / oc->obj_size;
}

static void set_default_params(struct flatfs_oc *oc) {
    oc->nr_obj_move_global_to_cpu = max(DIV_ROUND_UP(64 * 1024, oc->obj_size), 8ull);
    oc->cpu_cache_gc_threshold = max(DIV_ROUND_UP(256 * 1024, oc->obj_size), 64ull);
    oc->global_cache_gc_threshold = max(DIV_ROUND_UP(512 * 1024, oc->obj_size), 128ull);
}

static void insert_into_desc(struct flatfs_oc_desc *desc, struct flatfs_oc *oc) {
    __le64 off = flatfs_address_to_offset(oc->sb, oc);
    spin_lock(desc->lock);
    oc->next = desc->head;
    flatfs_memcpy_atomic(&desc->head, &off, sizeof(off));
    flatfs_flush_buffer(&desc->head, sizeof(off), true);
    spin_unlock(desc->lock);
}

static void remove_from_desc(struct flatfs_oc_desc *desc, struct flatfs_oc *oc) {
    __le64 off = flatfs_address_to_offset(oc->sb, oc);
    struct flatfs_oc *curr;
    spin_lock(desc->lock);
    for (curr = flatfs_offset_to_address(oc->sb, desc->head);
         curr->next && curr->next != off;
         curr = flatfs_offset_to_address(oc->sb, curr->next));
    BUG_ON(!curr->next);
    flatfs_memcpy_atomic(&curr->next, &oc->next, sizeof(curr->next));
    flatfs_flush_buffer(&curr->next, sizeof(curr->next), true);
    spin_unlock(desc->lock);
}

void *flatfs_oc_alloc(struct flatfs_oc *oc, gfp_t flags) {
    return cpu_alloc(oc, flags);
}

void flatfs_oc_free(struct flatfs_oc *oc, void *obj) {
    verify_free(oc, obj);
    cpu_free(oc, obj);
}

void flatfs_oc_reserve(struct flatfs_oc *oc, long nr_obj, gfp_t flags) {
    cpu_reserve(oc, nr_obj, flags);
}

static void fill_oc(struct super_block *sb, struct flatfs_oc *oc, const char *name, size_t obj_size, size_t align) {
    memset(oc->cpu_caches, 0, sizeof(oc->cpu_caches));
    memset(&oc->global_cache, 0, sizeof(oc->global_cache));

    BUG_ON(!obj_size);
    align = max(align, sizeof(__le64));
    obj_size = ALIGN(obj_size, align);

    BUG_ON(strlen(name) > OC_NAME_MAX_LEN);
    strcpy(oc->name, name);
    oc->sb = sb;
    oc->obj_size = obj_size;
    oc->data_off = ALIGN(sizeof(struct pslab_hdr), align);
    oc->global_cache.lock = kmalloc(sizeof(spinlock_t), GFP_ATOMIC);
    spin_lock_init(oc->global_cache.lock);

    set_pslab_info(oc);
    set_default_params(oc);

    oc->nr_balloc_action = oc->nr_bfree_action = 0;
}

void flatfs_oc_init(struct super_block *sb) {
    struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
    struct flatfs_oc_desc *desc;
    unsigned long blknr;
    void *start;
    __le64 off;

    if (flatfs_sb->s_oc_desc) {
        /* FIXME: flatfs_oc_recover */
        return;
    }

    flatfs_new_block(sb, &blknr, FLATFS_BLOCK_TYPE_2M, 0);
    start = flatfs_get_block(sb, flatfs_get_block_off(sb, blknr, FLATFS_BLOCK_TYPE_2M));
    desc = start;

    desc->head = 0;
    desc->lock = kmalloc(sizeof(spinlock_t), GFP_ATOMIC);
    spin_lock_init(desc->lock);

    fill_oc(sb, &desc->meta_oc, "meta-oc", sizeof(struct flatfs_oc), 0);
    insert_into_desc(desc, &desc->meta_oc);

    flatfs_flush_buffer(desc, sizeof(*desc), true);

    FLATFS_INFO(PSLAB_INIT, "Initialized scalable and persistent object-caching slab allocator.");

    off = flatfs_address_to_offset(sb, desc);
    flatfs_memcpy_atomic(&flatfs_sb->s_oc_desc, &off, sizeof(off));
    flatfs_flush_buffer(&flatfs_sb->s_oc_desc, sizeof(off), true);
}

struct flatfs_oc *flatfs_oc_create(struct super_block *sb, const char *name, size_t obj_size, size_t align) {
    struct flatfs_oc *meta_oc = get_meta_oc(sb), *oc;
    struct flatfs_oc_desc *desc = get_desc(sb);
    oc = flatfs_oc_alloc(meta_oc, 0);
    fill_oc(sb, oc, name, obj_size, align);
    flatfs_flush_buffer(oc, sizeof(*oc), true);
    insert_into_desc(desc, oc);
    return oc;
}

void flatfs_oc_destroy(struct flatfs_oc *oc) {
    struct flatfs_oc *meta_oc = get_meta_oc(oc->sb);
    struct flatfs_oc_desc *desc = get_desc(oc->sb);
    remove_from_desc(desc, oc);
    flatfs_oc_free(meta_oc, oc);
}

void flatfs_oc_dump(struct super_block *sb) {
    struct flatfs_oc_desc *desc = get_desc(sb);
    size_t nr_alloc_action, nr_free_action;
    struct flatfs_oc *oc;
    __le64 off;
    int cpu;

    printk("FlatFS Object-caching Slab Allocator Status:\n");

    spin_lock(desc->lock);

    for (off = desc->head; off; off = oc->next) {
        oc = flatfs_offset_to_address(sb, off);

        printk("  [%s]:\n", oc->name);
        printk("    Page size: %s\n", page_name((int) oc->pslab_page_type));
        printk("    Per-page chunk num: %llu\n", oc->nr_chunk_per_pslab);
        printk("    Global cache size: %llu chunks\n", oc->global_cache.nr_free);
        printk("    Object size: %llu bytes\n", oc->obj_size);
        printk("    Balloc action: %ld\n", oc->nr_balloc_action);
        printk("    Bfree action: %ld\n", oc->nr_bfree_action);

        nr_alloc_action = nr_free_action = 0;

        for (cpu = 0; cpu < FLATFS_NCPU; cpu++) {
            struct cpu_cache cpu_cache = oc->cpu_caches[cpu];

            printk("    CPU #%d:\n", cpu);
            printk("      Cache size: %llu chunks\n", cpu_cache.nr_free);
            printk("      Alloc action: %ld\n", cpu_cache.nr_alloc_action);
            printk("      Free action: %ld\n", cpu_cache.nr_free_action);

            nr_alloc_action += cpu_cache.nr_alloc_action;
            nr_free_action += cpu_cache.nr_free_action;
        }

        printk("    Total alloc action: %ld\n", nr_alloc_action);
        printk("    Total free action: %ld\n", nr_free_action);
        printk("    Not freed: %ld\n", nr_alloc_action - nr_free_action);
    }

    spin_unlock(desc->lock);
}
