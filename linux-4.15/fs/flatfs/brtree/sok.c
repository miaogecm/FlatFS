/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Scan-optimized Key Implementation
 */

#include <linux/kernel.h>
#include <linux/bug.h>
#include <linux/bitmap.h>

#include "../flatfs.h"
#include "pslab.h"
#include "sok.h"

struct off_page_oc_desc {
    const char *name;
    size_t size;
    struct flatfs_oc *oc;
};

static struct off_page_oc_desc off_page_oc_descs[] = {
    { "off-page-8",    8,    NULL },
    { "off-page-24",   24,   NULL },
    { "off-page-40",   40,   NULL },
    { "off-page-56",   56,   NULL },
    { "off-page-72",   72,   NULL },
    { "off-page-128",  128,  NULL },
    { "off-page-200",  200,  NULL },
    { "off-page-328",  328,  NULL },
    { "off-page-528",  528,  NULL },
    { "off-page-1024", 1024, NULL },
    { "off-page-2048", 2048, NULL },
    { "off-page-4104", 4104, NULL },
};

struct off_pg_to_free {
    sok_off_pg_t *off_pg;
    struct list_head list;
};

static struct kmem_cache *off_pg_to_free_oc;

#define NR_SEG_ARR_CACHE_LINES      (2 + DIV_ROUND_UP(SOK_SEG_SIZE * SOK_SEG_MAX_NR, CACHELINE_SIZE))

static void init_off_page_ocs(struct super_block *sb) {
    struct off_page_oc_desc *desc;
    int i;
    for (i = 0; i < ARRAY_SIZE(off_page_oc_descs); i++) {
        desc = &off_page_oc_descs[i];
        desc->oc = flatfs_oc_create(sb, desc->name, desc->size, 0);
    }
}

static inline struct off_page_oc_desc *get_off_page_oc_desc(size_t size) {
    int i;
    for (i = 0; i < ARRAY_SIZE(off_page_oc_descs); i++) {
        if (off_page_oc_descs[i].size >= size) {
            return &off_page_oc_descs[i];
        }
    }
    BUG();
}

static void init_off_pg_to_free_oc(void) {
    off_pg_to_free_oc = kmem_cache_create("off-pg-to-free", sizeof(struct off_pg_to_free), 0, SLAB_PANIC, NULL);
}

void brt_sok_init(struct super_block *sb) {
    init_off_page_ocs(sb);
    init_off_pg_to_free_oc();
}

static inline void flush_buffer_if_durable(sok_pool_t *pool, void *buf, uint32_t len, bool fence) {
    if (pool->durable) {
        flatfs_flush_buffer(buf, len, fence);
    }
}

static inline void flush_regions_if_durable(sok_pool_t *pool, void *start, unsigned long *victims, bool fence) {
    if (pool->durable) {
        flatfs_flush_regions(start, victims, fence);
    }
}

void sok_pool_init(sok_pool_t *pool, struct super_block *sb,
                   brt_tree_t *tree, brt_node_t *node,
                   void *seg_arr, int max_seg, int durable,
                   unsigned long *freemap, unsigned long *eidmap) {
    pool->sb = sb;
    pool->tree = tree;
    pool->node = node;
    pool->seg_arr = seg_arr;
    pool->max_seg = max_seg;
    pool->durable = durable;
    memcpy(pool->freemap, freemap, sizeof(pool->freemap));
    memcpy(pool->eidmap, eidmap, sizeof(pool->eidmap));
    INIT_LIST_HEAD(&pool->to_free);
}

sok_off_pg_t *sok_off_pg_alloc(sok_pool_t *pool, size_t size) {
    size_t alloc_size;
    void *new_off_pg;
    /* 2 bytes for @size field */
    size += 2;
    if (pool->durable) {
        struct off_page_oc_desc *desc;
        desc = get_off_page_oc_desc(size);
        new_off_pg = flatfs_oc_alloc(desc->oc, GFP_ATOMIC);
        alloc_size = desc->size;
    } else {
        new_off_pg = kmalloc(size, GFP_ATOMIC);
        alloc_size = size;
    }
    new_off_pg += alloc_size;
    ((u16 *) new_off_pg)[-1] = alloc_size - sizeof(__le16);
    return new_off_pg;
}

static void do_off_pg_free(sok_pool_t *pool, sok_off_pg_t *off_pg) {
    size_t size = ((u16 *) off_pg)[-1] + sizeof(__le16);
    struct off_page_oc_desc *desc;
    desc = get_off_page_oc_desc(size);
    flatfs_oc_free(desc->oc, off_pg - desc->size);
}

static void pool_free_off_pgs(sok_pool_t *pool) {
    struct off_pg_to_free *to_free, *tmp;
    
    list_for_each_entry_safe(to_free, tmp, &pool->to_free, list) {
        do_off_pg_free(pool, to_free->off_pg);
        list_del(&to_free->list);
        kmem_cache_free(off_pg_to_free_oc, to_free);
    }
}

sok_entry_t sok_copy(sok_pool_t *dst, int *eid, sok_pool_t *src, sok_entry_t ent) {
    sok_seg_t *segs[SOK_ENT_MAX_SEG] = { NULL };
    int nr_seg = sok_nr_seg(src, ent), i, id;

    for (i = 0; i < nr_seg; i++) {
        segs[i] = sok_seg_alloc(dst, &id);
        memcpy(segs[i], sok_id_to_seg(src, ent.ids[i]), SOK_SEG_SIZE);
    }

    *eid = sok_seg_eid(segs[0]);
    bitmap_set(dst->eidmap, *eid, 1);

    return sok_segs_to_entry(dst, segs);
}

sok_entry_t sok_alloc(sok_pool_t *pool, int *eid, size_t size, size_t segmentable_size) {
    int i, id, nr_seg = min_t(size_t, DIV_ROUND_UP(segmentable_size, SOK_SEG_DATA_SIZE), pool->max_seg);
    sok_seg_t *segs[SOK_ENT_MAX_SEG] = { NULL };
    unsigned long pos;
    u64 off_pg_off;

    if (size > nr_seg * SOK_SEG_DATA_SIZE) {
        off_pg_off = sok_off_pg_to_off(pool, sok_off_pg_alloc(pool, size - nr_seg * SOK_SEG_DATA_SIZE));
    } else {
        off_pg_off = SOK_SEG_NO_OFF_PG;
    }

    pos = find_first_zero_bit(pool->eidmap, SOK_ENT_MAX_NR);
    BUG_ON(pos >= SOK_ENT_MAX_NR);
    bitmap_set(pool->eidmap, pos, 1);
    *eid = (int) pos;

    for (i = 0; i < nr_seg; i++) {
        segs[i] = sok_seg_alloc(pool, &id);
        *(u64 *) segs[i] = sok_seg_hdr_word(segs, i, *eid, i ? SOK_SEG_NO_OFF_PG : off_pg_off);
    }

    return sok_segs_to_entry(pool, segs);
}

static void off_pg_free(sok_pool_t *pool, sok_off_pg_t *off_pg) {
    struct off_pg_to_free *to_free = kmem_cache_alloc(off_pg_to_free_oc, GFP_ATOMIC);
    to_free->off_pg = off_pg;
    INIT_LIST_HEAD(&to_free->list);
    list_add_tail(&to_free->list, &pool->to_free);
}

void sok_dealloc(sok_pool_t *pool, sok_entry_t ent) {
    sok_seg_t *first = sok_id_to_seg(pool, ent.ids[0]);
    size_t off;
    int i;

    for (i = 0; i < SOK_ENT_MAX_SEG && ent.ids[i] != SOK_SEG_NONE; i++) {
        sok_seg_free(pool, ent.ids[i]);
    }

    off = sok_seg_off_pg_off(first);
    if (off != SOK_SEG_NO_OFF_PG) {
        sok_off_pg_t *off_pg = sok_off_to_off_pg(pool, off);
        off_pg_free(pool, off_pg);
    }

    bitmap_clear(pool->eidmap, sok_seg_eid(first), 1);
}

int sok_compact(sok_pool_t *pool, sok_seg_change_t *change_list) {
    int total = bitmap_weight(pool->freemap, SOK_SEG_MAX_NR), i, pos, nr_change = 0;
    DECLARE_BITMAP(victims, NR_SEG_ARR_CACHE_LINES) = { 0 };
    sok_seg_change_t *change;
    void *dst, *src;

    for (i = 0; i < total; i++) {
        if (unlikely(!test_bit(i, pool->freemap))) {
            pos = (int) find_last_bit(pool->freemap, SOK_SEG_MAX_NR);
            bitmap_set(pool->freemap, i, 1);
            bitmap_clear(pool->freemap, pos, 1);

            dst = sok_id_to_seg(pool, i);
            src = sok_id_to_seg(pool, pos);
            memcpy(dst, src, SOK_SEG_SIZE);
            flatfs_mark_region(pool->seg_arr, victims, dst, SOK_SEG_SIZE);

            change = &change_list[nr_change++];
            change->from_id = pos;
            change->to_id = i;
        }
    }

    flush_regions_if_durable(pool, pool->seg_arr, victims, true);

    return nr_change;
}

size_t sok_resize(sok_pool_t *pool, sok_entry_t ent, size_t req_size) {
    size_t actual_size = sok_sizeof(pool, ent), old_off_pg_size, new_off_pg_size;
    sok_off_pg_t *new_off_pg, *old_off_pg;
    u64 off_pg_off, hdr_word;
    sok_seg_t *seg0;
    char *dst, *src;

    if (req_size <= actual_size) {
        return actual_size;
    }

    seg0 = sok_id_to_seg(pool, ent.ids[0]);

    off_pg_off = sok_seg_off_pg_off(seg0);
    if (off_pg_off == SOK_SEG_NO_OFF_PG) {
        old_off_pg = NULL;
        old_off_pg_size = 0;
    } else {
        old_off_pg = sok_off_to_off_pg(pool, off_pg_off);
        old_off_pg_size = sok_off_pg_size(old_off_pg);
    }

    new_off_pg = sok_off_pg_alloc(pool, old_off_pg_size + req_size - actual_size);
    new_off_pg_size = sok_off_pg_size(new_off_pg);
    if (old_off_pg) {
        dst = sok_off_pg_start(new_off_pg) - old_off_pg_size;
        src = sok_off_pg_str(old_off_pg);
        memcpy(dst, src, old_off_pg_size);
        flush_buffer_if_durable(pool, dst, old_off_pg_size + sizeof(u16), true);
    } else {
        flush_buffer_if_durable(pool, sok_off_pg_start(new_off_pg), sizeof(u16), true);
    }

    hdr_word = sok_seg_hdr_word(seg0, 0, sok_seg_eid(seg0), sok_off_pg_to_off(pool, new_off_pg));
    flatfs_memcpy_atomic(seg0, &hdr_word, sizeof(hdr_word));
    flush_buffer_if_durable(pool, seg0, sizeof(hdr_word), true);

    return new_off_pg_size;
}

void sok_prealloc(struct super_block *sb, int nr) {
    int i;
    for (i = 0; i < ARRAY_SIZE(off_page_oc_descs); i++) {
        /* Each entry has at most one off page. */
        flatfs_oc_reserve(off_page_oc_descs[i].oc, nr, 0);
    }
}

struct chunk {
    size_t start, end;
    bool is_off_pg;
    char *str;
};

#define CHUNK_MAX_NR        (1 + SOK_ENT_MAX_SEG)

static inline int get_chunks(struct chunk *chunks, sok_pool_t *pool, sok_entry_t ent, size_t start, size_t len) {
    size_t chunk_start, chunk_end = 0, end = start + len, s, e;
    sok_seg_t *segs[SOK_ENT_MAX_SEG];
    struct chunk *chunk = chunks;
    sok_off_pg_t *off_pg;
    u64 off;
    int i;

    sok_entry_to_segs(pool, segs, ent);

    for (i = SOK_ENT_MAX_SEG - 1; i >= 0; i--) {
        if (!segs[i]) {
            continue;
        }

        chunk_start = chunk_end;
        chunk_end += SOK_SEG_DATA_SIZE;

        s = max(start, chunk_start);
        e = min(end, chunk_end);

        if (s >= e) {
            continue;
        }

        len -= e - s;
        *chunk++ = (struct chunk) {
            .start     = s,
            .end       = e,
            .is_off_pg = false,
            .str       = sok_seg_str(segs[i]) + chunk_end - e
        };
    }

    off = sok_seg_off_pg_off(segs[0]);
    if (off == SOK_SEG_NO_OFF_PG) {
        goto out;
    }

    off_pg = sok_off_to_off_pg(pool, off);

    chunk_start = chunk_end;

    s = max(start, chunk_start);
    e = end;

    if (s >= e) {
        goto out;
    }

    len -= e - s;
    *chunk++ = (struct chunk) {
        .start     = s,
        .end       = e,
        .is_off_pg = true,
        .str       = sok_off_pg_start(off_pg) + chunk_start - e
    };

out:
    BUG_ON(len > 0);
    return (int) (chunk - chunks);
}

static void mark_flush_entry(sok_pool_t *pool, sok_entry_t ent, unsigned long *victims) {
    sok_seg_t *segs[SOK_ENT_MAX_SEG];
    sok_off_pg_t *off_pg;
    size_t off;
    int i;

    sok_entry_to_segs(pool, segs, ent);
    for (i = 0; i < SOK_ENT_MAX_SEG && segs[i]; i++) {
        flatfs_mark_region(pool->seg_arr, victims, segs[i], SOK_SEG_SIZE);
    }

    off = sok_seg_off_pg_off(segs[0]);
    if (off == SOK_SEG_NO_OFF_PG) {
        return;
    }

    off_pg = sok_off_to_off_pg(pool, off);
    flush_buffer_if_durable(pool, sok_off_pg_str(off_pg), sok_off_pg_size(off_pg) + sizeof(u16), false);
}

void sok_flush_entry(sok_pool_t *pool, sok_entry_t ent) {
    DECLARE_BITMAP(victims, NR_SEG_ARR_CACHE_LINES) = { 0 };
    mark_flush_entry(pool, ent, victims);
    flush_regions_if_durable(pool, pool->seg_arr, victims, false);
}

void sok_flush_entries(sok_pool_t *pool, int nr_ents, sok_entry_t *ents) {
    DECLARE_BITMAP(victims, NR_SEG_ARR_CACHE_LINES) = { 0 };
    int i;
    for (i = 0; i < nr_ents; i++) {
        mark_flush_entry(pool, ents[i], victims);
    }
    flush_regions_if_durable(pool, pool->seg_arr, victims, false);
}

void sok_flush_range(sok_pool_t *pool, sok_entry_t ent, size_t off, size_t len) {
    DECLARE_BITMAP(victims, NR_SEG_ARR_CACHE_LINES) = { 0 };
    struct chunk chunks[CHUNK_MAX_NR];
    int i, nr_chunks;
    nr_chunks = get_chunks(chunks, pool, ent, off, len);
    for (i = nr_chunks - 1; i >= 0; i--) {
        len = chunks[i].end - chunks[i].start;
        if (!chunks[i].is_off_pg) {
            flatfs_mark_region(pool->seg_arr, victims, chunks[i].str, len);
        } else {
            flush_buffer_if_durable(pool, chunks[i].str, len, false);
        }
    }
    flush_regions_if_durable(pool, pool->seg_arr, victims, false);
}

void sok_commit(sok_pool_t *pool) {
    pool_free_off_pgs(pool);
}

void *sok_get_ptr(sok_pool_t *pool, sok_entry_t ent, size_t off) {
    size_t chunk_end = 0, chunk_start;
    sok_seg_t *segs[SOK_ENT_MAX_SEG];
    sok_off_pg_t *off_pg;
    int i;

    sok_entry_to_segs(pool, segs, ent);

    for (i = SOK_ENT_MAX_SEG - 1; i >= 0; i--) {
        if (!segs[i]) {
            continue;
        }

        chunk_end += SOK_SEG_DATA_SIZE;

        if (off < chunk_end) {
            return sok_seg_str(segs[i]) + chunk_end - off;
        }
    }

    off = sok_seg_off_pg_off(segs[0]);
    if (off == SOK_SEG_NO_OFF_PG) {
        return NULL;
    }

    off_pg = sok_off_to_off_pg(pool, off);

    chunk_start = chunk_end;

    return sok_off_pg_start(off_pg) + chunk_start - off;
}

void sok_memcpy_to(sok_pool_t *pool, sok_entry_t ent, size_t dst, void *src, size_t len) {
    struct chunk chunks[CHUNK_MAX_NR];
    int i, nr_chunks;
    nr_chunks = get_chunks(chunks, pool, ent, dst, len);
    for (i = nr_chunks - 1; i >= 0; i--) {
        len = chunks[i].end - chunks[i].start;
        memcpy(chunks[i].str, src, len);
        src += len;
    }
}

void sok_memcpy_from(sok_pool_t *pool, void *dst, sok_entry_t ent, size_t src, size_t len) {
    struct chunk chunks[CHUNK_MAX_NR];
    int i, nr_chunks;
    nr_chunks = get_chunks(chunks, pool, ent, src, len);
    for (i = nr_chunks - 1; i >= 0; i--) {
        len = chunks[i].end - chunks[i].start;
        memcpy(dst, chunks[i].str, len);
        dst += len;
    }
}

int sok_strcmp(sok_pool_t *pool, fastr_t str, sok_entry_t ent, size_t *off, size_t len) {
    struct chunk chunks[CHUNK_MAX_NR];
    int i, nr_chunks, cmp = 0;
    unsigned long mismatch;
    size_t match = 0;
    fastr_t slice;

    nr_chunks = get_chunks(chunks, pool, ent, *off, len);

    for (i = nr_chunks - 1; i >= 0; i--) {
        len = chunks[i].end - chunks[i].start;
        slice = fastr(chunks[i].str, len);

        match += fastr_eliminate(&str, &slice).len;
        if ((mismatch = fastr_first_zpword(slice))) {
            cmp = fastr_wordcmp(fastr_first_zpword(str), mismatch);
            if (cmp) {
                goto out;
            }
        }
    }

    if (!fastr_is_empty(str)) {
        cmp = 1;
    }

out:
    *off += len - match;
    return cmp;
}

size_t sok_find_first(sok_pool_t *pool, sok_entry_t haystack, size_t off, size_t len, char needle) {
    struct chunk chunks[CHUNK_MAX_NR];
    size_t pos, chunk_len;
    int i, nr_chunks;
    fastr_t slice;

    nr_chunks = get_chunks(chunks, pool, haystack, off, len);

    for (i = 0; i < nr_chunks; i++) {
        chunk_len = chunks[i].end - chunks[i].start;
        slice = fastr(chunks[i].str, chunk_len);
        pos = fastr_find_last(slice, needle);
        if (pos != FASTR_NPOS) {
            return chunks[i].end - pos - 1;
        }
    }

    return off + len;
}

void sok_entry_stat(sok_pool_t *pool, int *nr_off_pgs, int *nr_segs, sok_entry_t ent, size_t off, size_t len) {
    struct chunk chunks[CHUNK_MAX_NR];
    int i, nr_chunks;
    *nr_off_pgs = 0;
    *nr_segs = 0;
    nr_chunks = get_chunks(chunks, pool, ent, off, len);
    for (i = 0; i < nr_chunks; i++) {
        if (chunks[i].is_off_pg) {
            (*nr_off_pgs)++;
        } else {
            (*nr_segs)++;
        }
    }
}
