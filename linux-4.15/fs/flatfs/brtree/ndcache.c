/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Node LRU Cache
 */

#include "../flatfs.h"
#include "ndcache.h"
#include "node.h"

#ifdef CONFIG_NDCACHE_STATISTICS
struct ndcache_stat ndcache_stat;
#endif

void ndcache_init(brt_tree_t *tree) {
    u8 i;
    int cpu;
    struct ndcache_entry *entry;
    for (cpu = 0; cpu < FLATFS_NCPU; cpu++) {
        struct ndcache_per_cpu *cache = &tree->ndc->u[cpu];
        INIT_LIST_HEAD(&cache->lru_list);
        for (i = 0; i < NDCACHE_SIZE; i++) {
            entry = &cache->nodes[i];
            entry->node = NULL;
            entry->vnode = NULL;
            INIT_LIST_HEAD(&entry->list);
            list_add_tail(&entry->list, &cache->lru_list);
        }
#ifdef CONFIG_NDCACHE_STATISTICS
        atomic_set(&ndcache_stat.hit, 0);
        atomic_set(&ndcache_stat.miss, 0);
        atomic_set(&ndcache_stat.n_cmp, 0);
#endif
    }
}

void ndcache_insert(brt_tree_t *tree, brt_node_t *node) {
    struct ndcache_entry *entry;
    struct ndcache_per_cpu *cache = &tree->ndc->u[smp_processor_id()];
    entry = list_last_entry(&cache->lru_list, struct ndcache_entry, list);
    if (entry->node) {
        brt_dnode_put(tree, entry->node);
    }
    brt_dnode_hold(tree, node);
    entry->node = node;
    entry->vnode = brt_vnode_get(node);
    list_move(&entry->list, &cache->lru_list);
#ifdef CONFIG_NDCACHE_STATISTICS
    atomic_inc(&ndcache_stat.miss);
#endif
}

void ndcache_access(brt_tree_t *tree, struct ndcache_entry *entry, brt_node_t *node) {
    struct ndcache_per_cpu *cache = &tree->ndc->u[smp_processor_id()];
    if (likely(entry->node == node)) {
        list_move(&entry->list, &cache->lru_list);
    }
#ifdef CONFIG_NDCACHE_STATISTICS
    atomic_inc(&ndcache_stat.hit);
#endif
}

void ndcache_flush(brt_tree_t *tree) {
    u8 i;
    int cpu;
    struct ndcache_entry *entry;
    for (cpu = 0; cpu < FLATFS_NCPU; cpu++) {
        struct ndcache_per_cpu *cache = &tree->ndc->u[cpu];
        for (i = 0; i < NDCACHE_SIZE; i++) {
            entry = &cache->nodes[i];
            if (entry->node) {
                brt_dnode_put(tree, entry->node);
                entry->node = NULL;
                entry->vnode = NULL;
            }
        }
    }
}
