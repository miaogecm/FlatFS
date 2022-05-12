/*
 * BRIEF DESCRIPTION
 *
 * File operations for files.
 *
 * Copyright 2012-2013 Intel Corporation
 * Copyright 2009-2011 Marco Stornelli <marco.stornelli@gmail.com>
 * Copyright 2003 Sony Corporation
 * Copyright 2003 Matsushita Electric Industrial Co., Ltd.
 * 2003-2004 (c) MontaVista Software, Inc. , Steve Longerbeam
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
 
#include <linux/fs.h>
#include <linux/sched.h>
#include <linux/slab.h>
#include <linux/uio.h>
#include <linux/mm.h>
#include <linux/uaccess.h>
#include <linux/falloc.h>
#include <asm/mman.h>
#include "flatfs.h"
#include "dax.h"

static inline int flatfs_can_set_blocksize_hint(struct flatfs_inode *pi,
					       loff_t new_size)
{
	/* Currently, we don't deallocate data blocks till the file is deleted.
	 * So no changing blocksize hints once allocation is done. */
	if (le64_to_cpu(pi->root))
		return 0;
	return 1;
}

int flatfs_set_blocksize_hint(struct super_block *sb, struct flatfs_inode *pi,
		loff_t new_size)
{
	unsigned short block_type;

	if (!flatfs_can_set_blocksize_hint(pi, new_size))
		return 0;

	if (new_size >= 0x40000000) {   /* 1G */
		block_type = FLATFS_BLOCK_TYPE_1G;
		goto hint_set;
	}

	if (new_size >= 0x200000) {     /* 2M */
		block_type = FLATFS_BLOCK_TYPE_2M;
		goto hint_set;
	}

	/* defaulting to 4K */
	block_type = FLATFS_BLOCK_TYPE_4K;

hint_set:
	flatfs_dbg_verbose(
		"Hint: new_size 0x%llx, i_size 0x%llx, root 0x%llx\n",
		new_size, pi->i_size, le64_to_cpu(pi->root));
	flatfs_dbg_verbose("Setting the hint to 0x%x\n", block_type);
	flatfs_memunlock_inode(sb, pi);
	pi->i_blk_type = block_type;
	flatfs_memlock_inode(sb, pi);
	return 0;
}

static long flatfs_fallocate(struct file *file, int mode, loff_t offset,
			    loff_t len)
{
	struct inode *inode = file->f_path.dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	long ret = 0;
	unsigned long blocknr, blockoff;
	int num_blocks, blocksize_mask;
	struct flatfs_inode *pi;
	flatfs_transaction_t *trans;
	loff_t new_size;

	/* We only support the FALLOC_FL_KEEP_SIZE mode */
	if (mode & ~FALLOC_FL_KEEP_SIZE)
		return -EOPNOTSUPP;

	if (S_ISDIR(inode->i_mode))
		return -ENODEV;

	inode_lock(inode);

	new_size = len + offset;
	if (!(mode & FALLOC_FL_KEEP_SIZE) && new_size > inode->i_size) {
		ret = inode_newsize_ok(inode, new_size);
		if (ret)
			goto out;
	}

	pi = flatfs_get_inode(sb, inode->i_ino);
	if (!pi) {
		ret = -EACCES;
		goto out;
	}
	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES +
			MAX_METABLOCK_LENTRIES);
	if (IS_ERR(trans)) {
		ret = PTR_ERR(trans);
		goto out;
	}
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	/* Set the block size hint */
	flatfs_set_blocksize_hint(sb, pi, new_size);

	blocksize_mask = sb->s_blocksize - 1;
	blocknr = offset >> sb->s_blocksize_bits;
	blockoff = offset & blocksize_mask;
	num_blocks = (blockoff + len + blocksize_mask) >> sb->s_blocksize_bits;
	ret = flatfs_alloc_blocks(trans, inode, blocknr, num_blocks, true);

	inode->i_mtime = inode->i_ctime = current_time(inode);

	flatfs_memunlock_inode(sb, pi);
	if (ret || (mode & FALLOC_FL_KEEP_SIZE)) {
		pi->i_flags |= cpu_to_le32(FLATFS_EOFBLOCKS_FL);
	}

	if (!(mode & FALLOC_FL_KEEP_SIZE) && new_size > inode->i_size) {
		inode->i_size = new_size;
		pi->i_size = cpu_to_le64(inode->i_size);
	}
	pi->i_mtime = cpu_to_le32(inode->i_mtime.tv_sec);
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	flatfs_commit_transaction(sb, trans);

out:
	inode_unlock(inode);
	return ret;
}

static loff_t flatfs_llseek(struct file *file, loff_t offset, int origin)
{
	struct inode *inode = file->f_path.dentry->d_inode;
	int retval;

	if (origin != SEEK_DATA && origin != SEEK_HOLE)
		return generic_file_llseek(file, offset, origin);

	inode_lock(inode);
	switch (origin) {
	case SEEK_DATA:
		retval = flatfs_find_region(inode, &offset, 0);
		if (retval) {
			inode_unlock(inode);
			return retval;
		}
		break;
	case SEEK_HOLE:
		retval = flatfs_find_region(inode, &offset, 1);
		if (retval) {
			inode_unlock(inode);
			return retval;
		}
		break;
	}

	if ((offset < 0 && !(file->f_mode & FMODE_UNSIGNED_OFFSET)) ||
	    offset > inode->i_sb->s_maxbytes) {
		inode_unlock(inode);
		return -EINVAL;
	}

	if (offset != file->f_pos) {
		file->f_pos = offset;
		file->f_version = 0;
	}

	inode_unlock(inode);
	return offset;
}

/* This function is called by both msync() and fsync().
 * TODO: Check if we can avoid calling flatfs_flush_buffer() for fsync. We use
 * movnti to write data to files, so we may want to avoid doing unnecessary
 * flatfs_flush_buffer() on fsync() */
int flatfs_fsync(struct file *file, loff_t start, loff_t end, int datasync)
{
	/* Sync from start to end[inclusive] */
	struct address_space *mapping = file->f_mapping;
	struct inode *inode = mapping->host;
	loff_t isize;
	timing_t fsync_time;

	FLATFS_START_TIMING(fsync_t, fsync_time);
	/* if the file is not mmap'ed, there is no need to do clflushes */
	if (mapping_mapped(mapping) == 0)
		goto persist;

	end += 1; /* end is inclusive. We like our indices normal please ! */

	isize = i_size_read(inode);

	if ((unsigned long)end > (unsigned long)isize)
		end = isize;
	if (!isize || (start >= end))
	{
		flatfs_dbg_verbose("[%s:%d] : (ERR) isize(%llx), start(%llx),"
			" end(%llx)\n", __func__, __LINE__, isize, start, end);
		FLATFS_END_TIMING(fsync_t, fsync_time);
		return -ENODATA;
	}

	/* Align start and end to cacheline boundaries */
	start = start & CACHELINE_MASK;
	end = CACHELINE_ALIGN(end);
	do {
		sector_t block = 0;
		void *xip_mem;
		pgoff_t pgoff;
		loff_t offset;
		unsigned long nr_flush_bytes;

		pgoff = start >> PAGE_SHIFT;
		offset = start & ~PAGE_MASK;

		nr_flush_bytes = PAGE_SIZE - offset;
		if (nr_flush_bytes > (end - start))
			nr_flush_bytes = end - start;

		block = flatfs_find_data_block(inode, (sector_t)pgoff, FLATFS_DAX_WRITE);

		if (block) {
			xip_mem = flatfs_get_block(inode->i_sb, block);
			/* flush the range */
			atomic64_inc(&fsync_pages);
			flatfs_flush_buffer(xip_mem + offset, nr_flush_bytes, 0);
		} else {
			/* sparse files could have such holes */
			flatfs_dbg_verbose("[%s:%d] : start(%llx), end(%llx),"
			" pgoff(%lx)\n", __func__, __LINE__, start, end, pgoff);
			break;
		}

		start += nr_flush_bytes;
	} while (start < end);
persist:
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	FLATFS_END_TIMING(fsync_t, fsync_time);
	return 0;
}

/* This callback is called when a file is closed */
static int flatfs_flush(struct file *file, fl_owner_t id)
{
	int ret = 0;
	/* if the file was opened for writing, make it persistent.
	 * TODO: Should we be more smart to check if the file was modified? */
	if (file->f_mode & FMODE_WRITE) {
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
	}

	return ret;
}

#if 0
static unsigned long
flatfs_get_unmapped_area(struct file *file, unsigned long addr,
			unsigned long len, unsigned long pgoff,
			unsigned long flags)
{
	unsigned long align_size;
	struct vm_area_struct *vma;
	struct mm_struct *mm = current->mm;
	struct inode *inode = file->f_mapping->host;
	struct flatfs_inode *pi = flatfs_get_inode(inode->i_sb, inode->i_ino);
	struct vm_unmapped_area_info info;

	if (len > TASK_SIZE)
		return -ENOMEM;

	if (pi->i_blk_type == FLATFS_BLOCK_TYPE_1G)
		align_size = PUD_SIZE;
	else if (pi->i_blk_type == FLATFS_BLOCK_TYPE_2M)
		align_size = PMD_SIZE;
	else
		align_size = PAGE_SIZE;

	if (flags & MAP_FIXED) {
		/* FIXME: We could use 4K mappings as fallback. */
		if (len & (align_size - 1))
			return -EINVAL;
		if (addr & (align_size - 1))
			return -EINVAL;
		return addr;
	}

	if (addr) {
		addr = ALIGN(addr, align_size);
		vma = find_vma(mm, addr);
		if (TASK_SIZE - len >= addr &&
		    (!vma || addr + len <= vma->vm_start))
			return addr;
	}

	/*
	 * FIXME: Using the following values for low_limit and high_limit
	 * implicitly disables ASLR. Awaiting a better way to have this fixed.
	 */
	info.flags = 0;
	info.length = len;
	info.low_limit = TASK_UNMAPPED_BASE;
	info.high_limit = TASK_SIZE;
	info.align_mask = align_size - 1;
	info.align_offset = 0;
	return vm_unmapped_area(&info);
}
#endif

const struct file_operations flatfs_xip_file_operations = {
	.llseek			= flatfs_llseek,
	.read			= flatfs_dax_file_read,
	.write			= flatfs_dax_file_write,
//	.aio_read		= xip_file_aio_read,
//	.aio_write		= xip_file_aio_write,
//	.read_iter		= generic_file_read_iter,
//	.write_iter		= generic_file_write_iter,
	.mmap			= flatfs_dax_file_mmap,
	.open			= generic_file_open,
	.fsync			= flatfs_fsync,
	.flush			= flatfs_flush,
//	.get_unmapped_area	= flatfs_get_unmapped_area,
	.unlocked_ioctl		= flatfs_ioctl,
	.fallocate		= flatfs_fallocate,
#ifdef CONFIG_COMPAT
	.compat_ioctl		= flatfs_compat_ioctl,
#endif
};

const struct inode_operations flatfs_file_inode_operations = {
	.setattr	= flatfs_setattr,
	.getattr	= flatfs_getattr,
	.get_acl	= NULL,
};
