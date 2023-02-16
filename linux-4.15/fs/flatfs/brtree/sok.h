/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Scan-optimized Key Implementation
 *
 * SoK provides 4 transactional management interfaces and 3 access interfaces
 * for higher layers.
 * Management interfaces: alloc, dealloc, compact, resize
 * Access interfaces: sizeof, memcpy_from, memcpy_to, prefetch
 */

#ifndef BRTREE_SOK_H
#define BRTREE_SOK_H

#include "tree.h"

/*
 * Segment: 7B Header + 33B Data
 * Header: 2b Order Bit + 6b EID + 6B Off Page Offset
 */

#define SOK_SEG_SIZE        BRT_SEG_SIZE
#define SOK_SEG_HDR_SIZE    7
#define SOK_SEG_DATA_SIZE   (SOK_SEG_SIZE - SOK_SEG_HDR_SIZE)

#define SOK_SEG_NONE        0xff
#define SOK_SEG_NO_OFF_PG   0xffffffffffff

#define SOK_ENT_MAX_NR      BRT_MAX_NR_ENT
#define SOK_ENT_MAX_SEG     BRT_DENT_NR_SEG
#define SOK_SEG_MAX_NR      (SOK_ENT_MAX_NR * SOK_ENT_MAX_SEG)

typedef struct sok_pool sok_pool_t;

typedef void sok_seg_t;
typedef void sok_off_pg_t;

typedef struct {
    u8 ids[SOK_ENT_MAX_SEG];
} sok_entry_t;

typedef struct {
    u8 from_id, to_id;
} sok_seg_change_t;

static inline int sok_seg_order(sok_seg_t *seg) {
    return *(u8 *) seg & 0xc0;
}

static inline int sok_seg_eid(sok_seg_t *seg) {
    return *(u8 *) seg & 0x3f;
}

static inline u64 sok_seg_off_pg_off(sok_seg_t *seg) {
    u64 off = 0;
    memcpy(&off, (u8 *) seg + 1, 6);
    return off;
}

static inline u64 sok_seg_hdr_word(sok_seg_t *seg, int order, int eid, u64 off_pg_off) {
    u64 word = *(u64 *) seg;
    *(u8 *) &word = (order << 6) | eid;
    memcpy((u8 *) &word + 1, &off_pg_off, 6);
    return word;
}

static inline char *sok_seg_start(sok_seg_t *seg) {
    return (char *) seg + SOK_SEG_SIZE;
}

static inline char *sok_seg_str(sok_seg_t *seg) {
    return (char *) seg + SOK_SEG_HDR_SIZE;
}

static inline fastr_t sok_seg_fastr(sok_seg_t *seg) {
    char *begin = sok_seg_str(seg);
    return fastr(begin, SOK_SEG_DATA_SIZE);
}

static inline size_t sok_off_pg_size(sok_off_pg_t *off_pg) {
    return ((u16 *) off_pg)[-1];
}

static inline char *sok_off_pg_start(sok_off_pg_t *off_pg) {
    return (char *) off_pg - sizeof(u16);
}

static inline char *sok_off_pg_str(sok_off_pg_t *off_pg) {
    return sok_off_pg_start(off_pg) - sok_off_pg_size(off_pg);
}

struct sok_pool {
    struct super_block *sb;

    brt_tree_t *tree;
    brt_node_t *node;

    void   *seg_arr;
    int     max_seg;
    bool    durable;

    DECLARE_BITMAP(freemap, SOK_SEG_MAX_NR);
    DECLARE_BITMAP(eidmap, SOK_ENT_MAX_NR);

    struct list_head to_free;
};

static inline int sok_seg_to_id(sok_pool_t *pool, sok_seg_t *seg) {
    return (int) (((char *) seg - (char *) pool->seg_arr) / SOK_SEG_SIZE);
}

static inline sok_seg_t *sok_id_to_seg(sok_pool_t *pool, int id) {
    return pool->seg_arr + SOK_SEG_SIZE * id;
}

static inline sok_seg_t *sok_seg_alloc(sok_pool_t *pool, int *id) {
    unsigned long pos;
    pos = find_first_zero_bit(pool->freemap, SOK_SEG_MAX_NR);
    BUG_ON(pos == SOK_SEG_MAX_NR);
    bitmap_set(pool->freemap, pos, 1);
    *id = (int) pos;
    return sok_id_to_seg(pool, *id);
}

static inline void sok_seg_free(sok_pool_t *pool, int id) {
    bitmap_clear(pool->freemap, id, 1);
}

static inline sok_entry_t sok_segs_to_entry(sok_pool_t *pool, sok_seg_t **segs) {
    sok_entry_t ent;
    int i;
    for (i = 0; i < SOK_ENT_MAX_SEG && segs[i]; i++) {
        ent.ids[i] = sok_seg_to_id(pool, segs[i]);
    }
    for (; i < SOK_ENT_MAX_SEG; i++) {
        ent.ids[i] = SOK_SEG_NONE;
    }
    return ent;
}

static inline void sok_entry_to_segs(sok_pool_t *pool, sok_seg_t **segs, sok_entry_t ent) {
    int i;
    memset(segs, 0, sizeof(*segs) * SOK_ENT_MAX_SEG);
    for (i = 0; i < SOK_ENT_MAX_SEG && ent.ids[i] != SOK_SEG_NONE; i++) {
        segs[i] = sok_id_to_seg(pool, ent.ids[i]);
    }
}

static inline int sok_nr_seg(sok_pool_t *pool, sok_entry_t ent) {
    int cnt;
    for (cnt = 0; cnt < SOK_ENT_MAX_SEG && ent.ids[cnt] != SOK_SEG_NONE; cnt++);
    return cnt;
}

sok_off_pg_t *sok_off_pg_alloc(sok_pool_t *pool, size_t size);

void sok_off_pg_free(sok_pool_t *pool, sok_off_pg_t *off_pg);

static inline u64 sok_off_pg_to_off(sok_pool_t *pool, sok_off_pg_t *new_off_pg) {
    if (pool->durable) {
        return flatfs_address_to_offset(pool->sb, new_off_pg);
    } else {
        return (u64) new_off_pg - FLATFS_VADDR_PRE;
    }
}

static inline sok_off_pg_t *sok_off_to_off_pg(sok_pool_t *pool, u64 off) {
    if (pool->durable) {
        return flatfs_offset_to_address(pool->sb, off);
    } else {
        return (sok_off_pg_t *) (off + FLATFS_VADDR_PRE);
    }
}

/*************************************************************
 * Management Interfaces
 * Note that these interfaces are transactional. Changes can
 * only take effect when freemap and eidmap is written.
 *************************************************************/

void sok_pool_init(sok_pool_t *pool, struct super_block *sb,
                   brt_tree_t *tree, brt_node_t *node,
                   void *seg_arr, int max_seg, int durable,
                   unsigned long *freemap, unsigned long *eidmap);

sok_entry_t sok_copy(sok_pool_t *dst, int *eid, sok_pool_t *src, sok_entry_t ent);

sok_entry_t sok_alloc(sok_pool_t *pool, int *eid, size_t size, size_t segmentable_size);

void sok_dealloc(sok_pool_t *pool, sok_entry_t ent);

int sok_compact(sok_pool_t *pool, sok_seg_change_t *change_list);

size_t sok_resize(sok_pool_t *pool, sok_entry_t ent, size_t req_size);

void sok_prealloc(struct super_block *sb, int nr);

void sok_flush_entry(sok_pool_t *pool, sok_entry_t ent);

void sok_flush_entries(sok_pool_t *pool, int nr_ents, sok_entry_t *ents);

void sok_flush_range(sok_pool_t *pool, sok_entry_t ent, size_t off, size_t len);

void sok_commit(sok_pool_t *pool);

static inline void sok_persistent_barrier(sok_pool_t *pool) {
    if (pool->durable) {
        PERSISTENT_BARRIER();
    }
}

/*************************************************************
 * Access Interfaces
 * Indexing:
 * /usr/local/parent/directory/path/to/filename
 *                 -----------------^
 *                        len      start
 *************************************************************/

static inline size_t sok_sizeof(sok_pool_t *pool, sok_entry_t ent) {
    sok_seg_t *segs[SOK_ENT_MAX_SEG];
    size_t size = 0;
    u64 off;
    int i;
    sok_entry_to_segs(pool, segs, ent);
    for (i = 0; i < SOK_ENT_MAX_SEG && segs[i]; i++) {
        size += sok_seg_fastr(segs[i]).len;
    }
    off = sok_seg_off_pg_off(segs[0]);
    if (off != SOK_SEG_NO_OFF_PG) {
        size += sok_off_pg_size(sok_off_to_off_pg(pool, off));
    }
    return size;
}

static inline int sok_get_nr_entries(sok_pool_t *pool) {
    return bitmap_weight(pool->eidmap, SOK_ENT_MAX_NR);
}

static inline bool sok_is_compact(sok_pool_t *pool) {
    unsigned long end = find_first_zero_bit(pool->freemap, SOK_SEG_MAX_NR);
    int nr = bitmap_weight(pool->freemap, SOK_SEG_MAX_NR);
    return nr == end;
}

static inline void sok_prefetch(sok_pool_t *pool) {
    int nr_seg = bitmap_weight(pool->freemap, SOK_SEG_MAX_NR);
    void *start = sok_id_to_seg(pool, 0);
    unsigned int len, off;
    BUG_ON(!sok_is_compact(pool));
	len = nr_seg * SOK_SEG_SIZE + ((unsigned long) (start) & (CACHELINE_SIZE - 1));
    for (off = 0; off < len; off += CACHELINE_SIZE) {
        prefetch(start + off);
    }
}

void *sok_get_ptr(sok_pool_t *pool, sok_entry_t ent, size_t off);

void sok_memcpy_to(sok_pool_t *pool, sok_entry_t ent, size_t dst, void *src, size_t len);

void sok_memcpy_from(sok_pool_t *pool, void *dst, sok_entry_t ent, size_t src, size_t len);

int sok_strcmp(sok_pool_t *pool, fastr_t str, sok_entry_t ent, size_t *off, size_t len);

size_t sok_find_first(sok_pool_t *pool, sok_entry_t haystack, size_t off, size_t len, char needle);

void sok_entry_stat(sok_pool_t *pool, int *nr_off_pgs, int *nr_segs, sok_entry_t ent, size_t off, size_t len);

#endif
