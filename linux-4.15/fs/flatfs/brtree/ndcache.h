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

#ifndef BPLUSTREE_RANGE_NDCACHE_H
#define BPLUSTREE_RANGE_NDCACHE_H

#include "../flatfs.h"

#define NDCACHE_SIZE   3

struct ndcache {
    struct ndcache_per_cpu {
        struct list_head lru_list;

        struct ndcache_entry {
            brt_node_t *node;
            brt_vnode_t *vnode;
            struct list_head list;
        } nodes[NDCACHE_SIZE];
    } u[FLATFS_NCPU];
};

#ifdef CONFIG_NDCACHE_STATISTICS
extern struct ndcache_stat {
    atomic_t hit, miss, n_cmp;
} ndcache_stat;
#endif

#define ndcache_for_each_entry(ndc, pos, body) \
    {                                          \
        struct ndcache_per_cpu *__c = &(ndc)->u[smp_processor_id()];        \
        list_for_each_entry(pos, &__c->lru_list, list) { if ((pos)->node) { \
            body                     \
        } }   \
    }

#ifdef CONFIG_NDCACHE_STATISTICS
#define ndcache_compare()       do { atomic_inc(&ndcache_stat.n_cmp); } while (0)
#else
#define ndcache_compare()       do { } while (0)
#endif

void ndcache_init(brt_tree_t *tree);
void ndcache_insert(brt_tree_t *tree, brt_node_t *node);
void ndcache_access(brt_tree_t *tree, struct ndcache_entry *entry, brt_node_t *node);
void ndcache_flush(brt_tree_t *tree);

#endif //BPLUSTREE_RANGE_NDCACHE_H
