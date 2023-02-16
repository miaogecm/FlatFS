/*
 * BRIEF DESCRIPTION
 *
 * Symlink operations
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
#include <linux/namei.h>

#include "flatfs.h"

int flatfs_block_symlink(struct inode *inode, const char *symname, int len)
{
	struct super_block *sb = inode->i_sb;
	u64 block;
	char *blockp;
	int err;

	err = flatfs_alloc_blocks(NULL, inode, 0, 1, false);
	if (err)
		return err;

	block = flatfs_find_data_block(inode, 0, FLATFS_DAX_WRITE);
	blockp = flatfs_get_block(sb, block);

	flatfs_memunlock_block(sb, blockp);
	memcpy(blockp, symname, len);
	blockp[len] = '\0';
	flatfs_memlock_block(sb, blockp);
	flatfs_flush_buffer(blockp, len + 1, false);
	
	return 0;
}

/* FIXME: Temporary workaround */
static int flatfs_readlink_copy(char __user *buffer, int buflen, const char *link)
{
	int len = PTR_ERR(link);
	if (IS_ERR(link))
		goto out;

	len = strlen(link);
	if (len > (unsigned) buflen)
		len = buflen;
	if (copy_to_user(buffer, link, len))
		len = -EFAULT;
    if (copy_to_user(&buffer[len], "\0", 1))
        len = -EFAULT;

#ifdef CONFIG_FLATFS_DEBUG
	printk("flatfs_readlink: %.*s\n", len, link);
#endif
	
out:
	return len;
}

static int flatfs_readlink(struct dentry* dentry, char __user *buffer, int buflen)
{
	struct inode *inode = dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	u64 block;
	char *blockp;

    if (unlikely(!flatfs_inode_is_symlink(inode))) {
        return -EINVAL;
    }

	block = flatfs_find_data_block(inode, 0, FLATFS_DAX_READ);
	blockp = flatfs_get_block(sb, block);
	
	return flatfs_readlink_copy(buffer, buflen, blockp);
}

const char *__flatfs_get_link(struct inode *inode) {
	struct super_block *sb = inode->i_sb;
	off_t block;
	char *blockp;

	block = flatfs_find_data_block(inode, 0, FLATFS_DAX_READ);
	blockp = flatfs_get_block(sb, block);
	return blockp;
}

static const char *flatfs_get_link(struct dentry *dentry, struct inode *inode, struct delayed_call *done) {
    return __flatfs_get_link(inode);
}

const struct inode_operations flatfs_symlink_inode_operations = {
	.readlink	= flatfs_readlink,
    .get_link   = flatfs_get_link,
	.setattr	= flatfs_setattr,
	.getattr	= flatfs_getattr,
};
