/*
 * BRIEF DESCRIPTION
 *
 * Memory protection definitions for the FLATFS filesystem.
 *
 * Copyright 2012-2013 Intel Corporation
 * Copyright 2010-2011 Marco Stornelli <marco.stornelli@gmail.com>
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
 
#ifndef __WPROTECT_H
#define __WPROTECT_H

#include <linux/fs.h>
#include "flatfs_def.h"

/* flatfs_memunlock_super() before calling! */
static inline void flatfs_sync_super(struct flatfs_super_block *ps)
{
	u16 crc = 0;

	ps->s_wtime = cpu_to_le32(get_seconds());
	ps->s_sum = 0;
	crc = crc16(~0, (__u8 *)ps + sizeof(__le16),
			FLATFS_SB_STATIC_SIZE(ps) - sizeof(__le16));
	ps->s_sum = cpu_to_le16(crc);
	/* Keep sync redundant super block */
	memcpy((void *)ps + FLATFS_SB_SIZE, (void *)ps,
		sizeof(struct flatfs_super_block));
}

#if 0
/* flatfs_memunlock_inode() before calling! */
static inline void flatfs_sync_inode(struct flatfs_inode *pi)
{
	u16 crc = 0;

	pi->i_sum = 0;
	crc = crc16(~0, (__u8 *)pi + sizeof(__le16), FLATFS_INODE_SIZE -
		    sizeof(__le16));
	pi->i_sum = cpu_to_le16(crc);
}
#endif

extern int flatfs_writeable(void *vaddr, unsigned long size, int rw);
extern int flatfs_dax_mem_protect(struct super_block *sb,
				 void *vaddr, unsigned long size, int rw);

static inline int flatfs_is_protected(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = (struct flatfs_sb_info *)sb->s_fs_info;

	return sbi->s_mount_opt & FLATFS_MOUNT_PROTECT;
}

static inline int flatfs_is_wprotected(struct super_block *sb)
{
	return flatfs_is_protected(sb);
}

static inline void
__flatfs_memunlock_range(void *p, unsigned long len)
{
	/*
	 * NOTE: Ideally we should lock all the kernel to be memory safe
	 * and avoid to write in the protected memory,
	 * obviously it's not possible, so we only serialize
	 * the operations at fs level. We can't disable the interrupts
	 * because we could have a deadlock in this path.
	 */
	flatfs_writeable(p, len, 1);
}

static inline void
__flatfs_memlock_range(void *p, unsigned long len)
{
	flatfs_writeable(p, len, 0);
}

static inline void flatfs_memunlock_range(struct super_block *sb, void *p,
					 unsigned long len)
{
	if (flatfs_is_protected(sb))
		__flatfs_memunlock_range(p, len);
}

static inline void flatfs_memlock_range(struct super_block *sb, void *p,
				       unsigned long len)
{
	if (flatfs_is_protected(sb))
		__flatfs_memlock_range(p, len);
}

static inline void flatfs_memunlock_super(struct super_block *sb,
					 struct flatfs_super_block *ps)
{
	if (flatfs_is_protected(sb))
		__flatfs_memunlock_range(ps, FLATFS_SB_SIZE);
}

static inline void flatfs_memlock_super(struct super_block *sb,
				       struct flatfs_super_block *ps)
{
	flatfs_sync_super(ps);
	if (flatfs_is_protected(sb))
		__flatfs_memlock_range(ps, FLATFS_SB_SIZE);
}

static inline void flatfs_memunlock_inode(struct super_block *sb,
					 struct flatfs_inode *pi)
{
	if (flatfs_is_protected(sb))
		__flatfs_memunlock_range(pi, FLATFS_SB_SIZE);
}

static inline void flatfs_memlock_inode(struct super_block *sb,
				       struct flatfs_inode *pi)
{
	/* flatfs_sync_inode(pi); */
	if (flatfs_is_protected(sb))
		__flatfs_memlock_range(pi, FLATFS_SB_SIZE);
}

static inline void flatfs_memunlock_block(struct super_block *sb, void *bp)
{
	if (flatfs_is_protected(sb))
		__flatfs_memunlock_range(bp, sb->s_blocksize);
}

static inline void flatfs_memlock_block(struct super_block *sb, void *bp)
{
	if (flatfs_is_protected(sb))
		__flatfs_memlock_range(bp, sb->s_blocksize);
}

#endif

