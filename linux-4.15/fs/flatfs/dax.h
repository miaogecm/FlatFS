/*
 * BRIEF DESCRIPTION
 *
 * XIP operations.
 *
 * Copyright 2012-2013 Intel Corporation
 * Copyright 2009-2011 Marco Stornelli <marco.stornelli@gmail.com>
 * This file is licensed under the terms of the GNU General Public
 * License version 2. This program is licensed "as is" without any
 * warranty of any kind, whether express or implied.
 */
/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 * 
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */

int flatfs_get_dax_mem(struct address_space *, pgoff_t, int, void **,
		      unsigned long *, int);
ssize_t flatfs_dax_file_read(struct file *filp, char __user *buf, size_t len,
			    loff_t *ppos);
ssize_t flatfs_dax_file_write(struct file *filp, const char __user *buf,
		size_t len, loff_t *ppos);
int flatfs_dax_file_mmap(struct file *file, struct vm_area_struct *vma);

static inline int flatfs_use_xip(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	return sbi->s_mount_opt & FLATFS_MOUNT_XIP;
}

#define mapping_is_xip(map) (map->a_ops->get_dax_mem)

