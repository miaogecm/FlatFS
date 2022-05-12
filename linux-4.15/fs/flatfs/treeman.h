/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */

#ifndef TREE_MAN_H
#define TREE_MAN_H

#include "flatfs.h"

brt_node_t *__alloc_bplus_tree_node(brt_tree_t *tree, brt_ndtype_t type);
void __free_bplus_tree_node(brt_tree_t *tree, brt_node_t *node);

void __free_bplus_entry_buffer(struct super_block *sb, void* entry);
void* __alloc_bplus_entry_buffer(brt_tree_t * tree, brt_node_t * node, size_t esize);

#define brt_nd_new      __alloc_bplus_tree_node
#define brt_nd_free     __free_bplus_tree_node
#define brt_entbuf_new  __alloc_bplus_entry_buffer
#define brt_entbuf_free __free_bplus_entry_buffer

#define ENTER_BLOCK_MANIPULATION    ((void) 0)
#define LEAVE_BLOCK_MANIPULATION    ((void) 0)
#define BEFORE_BLOCK_PREALLOCATION  ((void) 0)
#define AFTER_BLOCK_PREALLOCATION   ((void) 0)

int deinit_node_block_list(struct flatfs_sb_info* sbi);
int deinit_entry_block_list(struct flatfs_sb_info* sbi);
int init_node_block_lists(struct flatfs_sb_info* sbi);
int init_entry_block_lists(struct flatfs_sb_info* sbi);

#endif
