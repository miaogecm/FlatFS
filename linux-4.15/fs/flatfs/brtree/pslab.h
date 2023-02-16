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

#ifndef FLATFS_PSLAB_H
#define FLATFS_PSLAB_H

#include <linux/fs.h>

struct flatfs_oc;
struct flatfs_oc_desc;

void flatfs_oc_init(struct super_block *sb);
struct flatfs_oc *flatfs_oc_create(struct super_block *sb, const char *name, size_t obj_size, size_t align);
void flatfs_oc_destroy(struct flatfs_oc *oc);
void *flatfs_oc_alloc(struct flatfs_oc *oc, gfp_t flags);
void flatfs_oc_free(struct flatfs_oc *oc, void *obj);
void flatfs_oc_reserve(struct flatfs_oc *oc, long nr_obj, gfp_t flags);
void flatfs_oc_dump(struct super_block *sb);

#endif //FLATFS_PSLAB_H
