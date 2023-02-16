/*
 * BRIEF DESCRIPTION
 *
 * Inode operations for directories.
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
 * FlatFS namespace consistency technique. FlatFS adopts two-layer transactions: FS and Br-tree
 * transaction management. Br-tree provides crash-consistency tree operations. FS transaction
 * guarantees metadata consistency of file system layer. Specifically,
 *
 * 1. creat, mkdir, link, symlink: these system calls involve creating an inode and insert
 * a br-tree entry. To guarantee namespace consistency, the correct ordering is (1) start a FS
 * transaction, (2) create an inode, (3) commit FS transaction, (4) insert br-tree entry. 
 * If a crash happens between (1) and (3), FlatFS undo all changes. If a crash happens between (3)
 * and (4), FlatFS undo all changes in br-tree. However, it also causes memory leakage for the newly
 * -created inode.
 * 
 * 2. unlink, rmdir: these system calls involve freeing an inode and removing a br-tree entry. The 
 * correct ordering is (1) remove br-tree entry, (2) start a FS transaction, (3) free an inode, (4)
 * commit FS transaction. If a crash happens after (1), it also causes memory leakage for the inode. 
 * However, this inode disappears in the FlatFS namespace tree. The user is unable to find this inode.
 *
 * 3. rmdir_recur: remove a directory and all its entries recursively. FlatFS adopts a redo-like 
 * technique to ensure namespace consistency. rmdir_recur slices the namespace subtree for the removed
 * directory. Then, it free all inodes of directory entries. For each inode remove event, we create a  
 * transaction to achieve atomic durability. If there is a crash happens during inode free. During 
 * recovery, FlatFS utilizes the sliced sub-tree to continue the inode free.
 *
 * 4. cpdir_recur: copy a directory and all its entries recursively. FlatFS adopts a redo-like technique
 * to ensure namespace consistency. cpdir_recur first performs file system layer operation. Note that 
 * it also needs to log the directory pathname. 
 *
 * 5. rename: file rename involves removing old entry and inserting a new entry in br-tree. Currently,
 * we only guarantee the atomicity of single operation. Thus, we also log the old and new pathname of 
 * this rename operation. The transaction ordering of file renaming: (1) file system layer operation,
 * (2) namespace tree operation. 
 * Directory rename also logs the old and new pathname of this rename operation. The tree-layer operations
 * (i.e., range slice and insert) are carefully designed to support atomicity.
 */

#include <linux/fs.h>
#include <linux/slab.h>
#include <linux/fastr.h>
#include <linux/bitmap.h>
#include <linux/pathman.h>
#include <drm/drm_fixed.h>

#include "flatfs.h"
#include "dax.h"
#include "brtree/tree.h"

/*
 * Couple of helper functions - make the code slightly cleaner.
 */
static inline void flatfs_inc_count(struct inode *inode, struct flatfs_inode *pi)
{
	inc_nlink(inode);
	flatfs_update_nlink(inode, pi);
}

static inline void flatfs_dec_count(struct inode *inode, struct flatfs_inode *pi)
{
	if (inode->i_nlink) {
		drop_nlink(inode);
		flatfs_update_nlink(inode, pi);
	}
}

static int flatfs_create(struct inode *dir, struct dentry *dentry, umode_t mode, bool excl)
{
    struct fentry *fe = (struct fentry *) dentry;
	struct inode *inode = NULL, *inode2;
	struct super_block *sb = dir->i_sb;
	struct flatfs_inode* pi;
	flatfs_transaction_t *trans;
	timing_t create_time;
	long ino;
	int err = 0;

    BUG_ON(!d_in_flat_ns(dentry));

	FLATFS_START_TIMING(create_t, create_time);
	/* two log entries for new inode, 1 lentry for dir inode, 1 for dir
	 * inode's b-tree, 2 lentries for logging dir entry
	 */
	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		goto out_err;
	}

	/* allocate a new VFS & flatfs inode */
	inode = flatfs_new_inode(trans, dir, mode);
	if (IS_ERR(inode)) {
		flatfs_abort_transaction(sb, trans);
        err = PTR_ERR(inode);
		goto out_err;
	}
#ifdef CONFIG_FLATFS_DEBUG
	printk("flatfs_new_inode: path %.*s, ino %lu\n", FASTR_FMT(fe->d_fullpath), inode->i_ino);
#endif
	
	inode->i_op = &flatfs_file_inode_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;
	inode->i_fop = &flatfs_xip_file_operations;

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_FILE;
	flatfs_memlock_inode(sb, pi);

	flatfs_commit_transaction(sb, trans);

	/* insert into B+ tree */
	ino = flatfs_namespace_insert(tree_of(fe), fe->d_fullpath, (inode->i_ino >> FLATFS_INODE_BITS),
                                  fe->d_ppcs, 0);
	if (ino > 0) {
		inode2 = flatfs_iget(sb, ino << FLATFS_INODE_BITS);
		iput(inode2);
		goto out_clear_inode;
	} else if (ino < 0)
		goto out_err;

#ifdef CONFIG_FLATFS_DEBUG
	printk("bplus_tree_insert: path %.*s, ino %lu\n", FASTR_FMT(fe->d_fullpath), inode->i_ino);
#endif

	unlock_new_inode(inode);

	FLATFS_END_TIMING(create_t, create_time);
    fe->d_inode = inode;
    fe->d_flags |= DCACHE_REGULAR_TYPE;
	return 0;
out_clear_inode:
	unlock_new_inode(inode);
	clear_nlink(inode);
	iput(inode);
    fe->d_inode = inode2;
    fe->d_flags |= DCACHE_REGULAR_TYPE;
	return 0;
out_err:
	return err;
}

static int flatfs_mknod(struct inode *dir, struct dentry *dentry, umode_t mode,
		       dev_t rdev)
{
	struct inode *inode = NULL;
	int err = PTR_ERR(inode);
	flatfs_transaction_t *trans;
	struct super_block *sb = dir->i_sb;
	struct flatfs_inode *pi;

	/* 2 log entries for new inode, 1 lentry for dir inode, 1 for dir
	 * inode's b-tree, 2 lentries for logging dir entry
	 */
	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		goto out;
	}

	inode = flatfs_new_inode(trans, dir, mode);
	if (IS_ERR(inode))
		goto out_err;
	init_special_inode(inode, mode, rdev);
	inode->i_op = &flatfs_special_inode_operations;

	pi = flatfs_get_inode(sb, inode->i_ino);
	if (S_ISCHR(inode->i_mode) || S_ISBLK(inode->i_mode))
		pi->dev.rdev = cpu_to_le32(inode->i_rdev);
	
	if (err)
		goto out_err;
	flatfs_commit_transaction(sb, trans);
out:
	return err;
out_err:
	flatfs_abort_transaction(sb, trans);
	return err;
}

static int flatfs_symlink(struct inode *dir, struct dentry *dentry, const char *symname)
{
    struct fentry *fe = (struct fentry *) dentry;
	struct super_block *sb = dir->i_sb;
	int ret = 0, len = strlen(symname);
	struct inode *inode = NULL;
	flatfs_transaction_t *trans;
	struct flatfs_inode *pi;
    fastr_t rewalk_key;
    long err;

    BUG_ON(!d_in_flat_ns(dentry));

	FLATFS_INFO(FS_SYMLINK, "%.*s --> %s", FASTR_FMT(fe->d_fullpath), symname);

	if (len + 1 > sb->s_blocksize) {
        ret = -ENAMETOOLONG;
        goto out;
    }

	/* 2 log entries for new inode, 1 lentry for dir inode, 1 for dir
	 * inode's b-tree, 2 lentries for logging dir entry
	 */
	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2 +
			MAX_DIRENTRY_LENTRIES);
	if (IS_ERR(trans)) {
		ret = PTR_ERR(trans);
		goto out;
	}

	inode = flatfs_new_inode(trans, dir, S_IFLNK|S_IRWXUGO);
	if (IS_ERR(inode)) {
        ret = PTR_ERR(inode);
		goto out_abort_trans;
	}

	inode->i_op = &flatfs_symlink_inode_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;

	pi = flatfs_get_inode(sb, inode->i_ino);
	pi->i_types = FLATFS_INODE_TYPE_SYMLINK;
	ret = flatfs_block_symlink(inode, symname, len);
	if (ret)
		goto out_free_inode;

	inode->i_size = len;
	flatfs_update_isize(inode, pi);

	flatfs_commit_transaction(sb, trans);
    trans = NULL;

	/* insert new key-value pair */
	err = flatfs_namespace_insert(tree_of(fe), fe->d_fullpath,
                                  (inode->i_ino >> FLATFS_INODE_BITS) | INO_ANNOTATE_SYMLINK,
                                  fe->d_ppcs, 0);
	if (err) {
        ret = err > 0 ? -EEXIST : (int) err;
		goto out_free_inode;
	}
	unlock_new_inode(inode);

    fe->d_flags |= DCACHE_SYMLINK_TYPE;
    fe->d_inode = inode;

	FLATFS_INFO(FS_SYMLINK, "bplus_tree_insert: %.*s inode[%lu]", FASTR_FMT(fe->d_fullpath), inode->i_ino);

    rewalk_key = fastr(kmalloc(fe->d_fullpath.len + 2, GFP_ATOMIC), 0);
    fastr_append(&rewalk_key, fe->d_fullpath);
    fastr_append_ch(&rewalk_key, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append_ch(&rewalk_key, CTLCHR_PREFIX_INTERCEPTOR);

    /* insert rewalk entry */
    err = flatfs_namespace_insert(tree_of(fe), rewalk_key,
                                  (inode->i_ino >> FLATFS_INODE_BITS) | INO_ANNOTATE_SYMLINK,
                                  fe->d_ppcs, ino2pppc(inode));

    kfree(rewalk_key.chars);

    if (err) {
        ret = err > 0 ? -EEXIST : (int) err;
        goto out_free_inode;
    }

    FLATFS_INFO(FS_SYMLINK, "bplus_tree_insert: (rewalk entry) %.*s -- > %s inode[%lu] %016lx",
                FASTR_FMT(fe->d_fullpath), symname, inode->i_ino,
                (inode->i_ino >> FLATFS_INODE_BITS) | INO_ANNOTATE_SYMLINK);

out_free_inode:
    if (unlikely(ret)) {
        flatfs_dec_count(inode, pi);
        unlock_new_inode(inode);
        iput(inode);
    }

out_abort_trans:
	flatfs_abort_transaction(sb, trans);

out:
	return ret;
}

static int flatfs_link(struct dentry *old_dentry, struct inode *dir, struct dentry *dentry)
{
    struct fentry *fe = (struct fentry *) dentry;
    struct inode *inode = old_dentry->d_inode;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_transaction_t *trans;
	int err = 0;

    BUG_ON(!d_in_flat_ns(dentry));

	FLATFS_INFO(FS_LINK, "%.*s --> inode[%lu]", FASTR_FMT(fe->d_fullpath), inode->i_ino);

	if (inode->i_nlink >= FLATFS_LINK_MAX)
		return -EMLINK;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2 +
			MAX_DIRENTRY_LENTRIES);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		goto out;
	}
	
	/* only need to log the first 48 bytes since we only modify ctime and
	 * i_links_count in this system call */
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	ihold(inode);

	inode->i_ctime = current_time(inode);
	inc_nlink(inode);

	flatfs_memunlock_inode(sb, pi);
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	flatfs_memlock_inode(sb, pi);

	flatfs_commit_transaction(sb, trans);

	/* insert the new key-value pair */
	err = flatfs_namespace_insert(tree_of(fe), fe->d_fullpath, (inode->i_ino >> FLATFS_INODE_BITS),
                                  fe->d_ppcs, 0);
    if (err > 0) {
        err = -EEXIST;
    }
    if (err) {
        goto out;
    }

    fe->d_flags |= DCACHE_REGULAR_TYPE;
    fe->d_inode = inode;

	FLATFS_INFO(FS_LINK, "bplus_tree_insert: %.*s inode[%lu]", FASTR_FMT(fe->d_fullpath), inode->i_ino);

out:
	return err;
}

static int flatfs_unlink(struct inode* dir, struct dentry *dentry)
{
    struct fentry *fe = (struct fentry *) dentry;
    struct inode *inode;
	struct super_block *sb = dir->i_sb;
	struct flatfs_inode *pi;
	timing_t unlink_time;
	flatfs_transaction_t *trans;
	int retval;

    BUG_ON(!d_in_flat_ns(dentry));

	FLATFS_INFO(FS_UNLINK, "%.*s", FASTR_FMT(fe->d_fullpath));

	FLATFS_START_TIMING(unlink_t, unlink_time);

	/* remove the key-value pair */
	retval = flatfs_namespace_remove(tree_of(fe), fe->d_fullpath);
	if (retval < 0) {
		retval = -ENOENT;
		goto out;
	}

    inode = dentry->d_inode;

    pi = flatfs_get_inode(sb, inode->i_ino);

	FLATFS_INFO(FS_UNLINK, "bplus_tree_remove: %.*s", FASTR_FMT(fe->d_fullpath));

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2 +
		MAX_DIRENTRY_LENTRIES);
	if (IS_ERR(trans)) {
		retval = PTR_ERR(trans);
		goto out;
	}
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	/* Is it a symlink to directory ? */
	if (unlikely(S_ISLNK(inode->i_mode))) {
		fastr_t rewalk_path;
		char buf[1024];
		
    	rewalk_path = fastr(buf, 0);
    	fastr_append(&rewalk_path, fe->d_fullpath);
		fastr_append_ch(&rewalk_path, CTLCHR_COMPONENT_SEPARATOR);
		fastr_append_ch(&rewalk_path, CTLCHR_PREFIX_INTERCEPTOR);

		FLATFS_INFO(FS_UNLINK, "remove rewalk entry: %.*s %.*s", FASTR_FMT(fe->d_fullpath), FASTR_FMT(rewalk_path));

		if (flatfs_namespace_remove(tree_under(fe), rewalk_path) < 0) {
			retval = -ENOENT;
			flatfs_abort_transaction(sb, trans);
			goto out;
		}
	}

	if (inode->i_nlink == 1)
		flatfs_truncate_add(inode, inode->i_size);
	inode->i_ctime = dir->i_ctime;

	flatfs_memunlock_inode(sb, pi);
	if (inode->i_nlink) {
		drop_nlink(inode);
		pi->i_links_count = cpu_to_le16(inode->i_nlink);
	}
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	flatfs_commit_transaction(sb, trans);
	FLATFS_END_TIMING(unlink_t, unlink_time);
	
out:
	return retval;
}

static int flatfs_mkdir(struct inode *dir, struct dentry *dentry, umode_t mode)
{
    struct fentry *fe = (struct fentry *) dentry;
	struct super_block *sb = dir->i_sb;
	struct flatfs_inode *pi, *pidir;
	flatfs_transaction_t *trans;
	struct inode *inode = NULL;
    int ret = 0;
    long err;

    BUG_ON(!d_in_flat_ns(dentry));

	if (dir->i_nlink >= FLATFS_LINK_MAX) {
        ret = -EMLINK;
        goto out;
    }

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2 +
			MAX_DIRENTRY_LENTRIES);
	if (IS_ERR(trans)) {
		ret = PTR_ERR(trans);
		goto out;
	}

	/* create inode for this new directory */
	inode = flatfs_new_inode(trans, dir, S_IFDIR | mode);
	if (IS_ERR(inode)) {
        ret = PTR_ERR(inode);
		goto out_abort_trans;
	}

	/* insert a new key-value pair into tree */
	err = flatfs_namespace_insert(tree_of(fe), fe->d_fullpath, (inode->i_ino >> FLATFS_INODE_BITS),
                                  fe->d_ppcs, 0);
	if (err) {
        ret = err > 0 ? -EEXIST : (int) err;
        goto out_free_inode;
    }

    fe->d_flags |= DCACHE_DIRECTORY_TYPE;
    fe->d_inode = inode;

	FLATFS_INFO(FS_MKDIR, "bplus_tree_insert: %.*s", FASTR_FMT(fe->d_fullpath));
	
	inode->i_op = &flatfs_dir_inode_operations;
    inode->i_flat_op = &flatfs_dir_inode_flat_operations;
	inode->i_fop = &flatfs_dir_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;

	/* since this is a new inode so we don't need to include this
	 * flatfs_alloc_blocks in the transaction
	 */
	inode->i_size = sb->s_blocksize;

	set_nlink(inode, 2);

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_DIR;
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_size = cpu_to_le64(inode->i_size);
	flatfs_memlock_inode(sb, pi);

	pidir = flatfs_get_inode(sb, dir->i_ino);
	flatfs_inc_count(dir, pidir);
	
	unlock_new_inode(inode);

	flatfs_commit_transaction(sb, trans);
    trans = NULL;

	FLATFS_INFO(FS_MKDIR, "ino[%lu] count[%d] %.*s",
                inode->i_ino, atomic_read(&inode->i_count), FASTR_FMT(fe->d_fullpath));

out_free_inode:
    if (unlikely(ret)) {
        clear_nlink(inode);
        unlock_new_inode(inode);
        iput(inode);
    }

out_abort_trans:
    flatfs_abort_transaction(sb, trans);

out:
	return ret;
}

/*
 * routine to check that the specified directory is empty
 * return 1 if empty
 */
static int flatfs_empty_dir(struct fentry *fe) {
    fastr_t path = fe->d_fullpath, next;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
    brt_it_t it;
    int ret;

    brt_scan(tree_under(fe), &it, path, FASTR_LITERAL("\x7f"));

    /* Skip the directory itself. */
    ret = brt_it_next(&it, &qr, 0);
    BUG_ON(ret);

    ret = brt_it_next(&it, &qr, 0);

    if (unlikely(ret == -ENOENT)) {
        ret = 1;
    } else {
        next = qr.path;
        ret = !fastr_is_prefix(next, path) || next.chars[path.len] != CTLCHR_COMPONENT_SEPARATOR;
    }

    brt_it_close(&it);
    return ret;
}

static int __flatfs_remove_file(struct super_block *sb, struct inode *dir,
		struct inode *inode, struct flatfs_inode *pi) {
	flatfs_transaction_t *trans;
	int err;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		return err;
	}

	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	if (inode->i_nlink == 1)
		flatfs_truncate_add(inode, inode->i_size);
	inode->i_ctime = dir->i_ctime;

	flatfs_memunlock_inode(sb, pi);
	if (inode->i_nlink) {
		drop_nlink(inode);
		pi->i_links_count = cpu_to_le16(inode->i_nlink);
	}
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	iput(inode); /* this iput for flatfs_iget */

	return 0;
}

static int __flatfs_remove_dir(struct super_block *sb, struct inode *dir,
		struct inode *inode, struct flatfs_inode *pi) {
	flatfs_transaction_t *trans;
	int err;
	
	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		return err;
	}

	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	/*inode->i_version++; */
	clear_nlink(inode);
	inode->i_ctime = dir->i_ctime;

	flatfs_memunlock_inode(sb, pi);
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	/* add the inode to truncate list in case a crash happens before the
	 * subsequent evict_inode is called. It will be deleted from the
	 * truncate list during evict_inode.
	 */
	flatfs_truncate_add(inode, inode->i_size);

	iput(inode); /* this iput for flatfs_iget */

	flatfs_commit_transaction(sb, trans);

	return 0;
}

void range_remove(struct super_block *sb, brt_tree_t *removed, brt_tree_t *tree, brt_key_t left, brt_key_t right) {
    brt_tree_t tmp = flatfs_get_tree(sb);
    brt_release(removed, tree, left);
    brt_release(&tmp, removed, right);
    brt_absorb(tree, &tmp);
    flatfs_put_tree(&tmp);
}

void range_insert(struct super_block *sb, brt_tree_t *dst, brt_tree_t *src) {
    brt_it_t sit, dit;
    DEFINE_BRT_QR(sqr, BRT_QR_RAW);
    DEFINE_BRT_QR(dqr, BRT_QR_RAW);
    int ret;

    brt_scan(src, &sit, FASTR_LITERAL("\0"), FASTR_LITERAL("\x7f"));
    ret = brt_it_next(&sit, &sqr, 0);
    BUG_ON(ret);
    brt_scan(dst, &dit, sqr.path, FASTR_LITERAL("\x7f"));
    brt_it_close(&sit);
    ret = brt_it_next(&dit, &dqr, 0);

    if (unlikely(ret == -ENOENT)) {
        /* src > dst, just append src to dst */
        brt_absorb(dst, src);
    } else if (!ret) {
        brt_tree_t tmp = flatfs_get_tree(sb);
        brt_release(&tmp, dst, dqr.path);
        brt_absorb(dst, src);
        brt_absorb(dst, &tmp);
        BUG_ON(!brt_is_empty(&tmp));
        flatfs_put_tree(&tmp);
    } else {
        BUG();
    }
    BUG_ON(!brt_is_empty(src));

    brt_it_close(&dit);
}

static int __flatfs_rmdir_recur(struct super_block *sb, struct domain *domain, struct inode *dir, fastr_t dirpath) {
	brt_tree_t *tree, subtree;
	int err = 0;
	struct flatfs_inode *pi;
	struct inode *inode;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
	fastr_t left, right;
	fastr_t path;
    brt_it_t it;
	ino_t ino;

    if (unlikely(!list_empty(&domain->children))) {
        return -ECHILD;
    }

	tree = domain->tree;

	/* remove all nodes in B+ tree */
    subtree = flatfs_get_tree(sb);
    left = dirpath;
    right = dir_path_upperbound(dirpath);
    range_remove(sb, &subtree, tree, left, right);
    kfree(right.chars);

    /* free inodes */
    brt_scan(&subtree, &it, FASTR_LITERAL("\0"), FASTR_LITERAL("\x7f"));
    while (!brt_it_next(&it, &qr, 0)) {
        ino = qr.inode;
        path = qr.path;

		pi = flatfs_get_inode(sb, le64_to_cpu(ino) << FLATFS_INODE_BITS);

		switch (pi->i_types) {
		case FLATFS_INODE_TYPE_DIR:
			inode = flatfs_iget(sb, le64_to_cpu(ino) << FLATFS_INODE_BITS);

			inode_lock(inode);
			err = __flatfs_remove_dir(sb, dir, inode, pi);
			inode_unlock(inode);

			if (err)
				return err;

			iput(inode); /* release the inode */
			break;
		case FLATFS_INODE_TYPE_FILE:
		case FLATFS_INODE_TYPE_HDLINK:
		case FLATFS_INODE_TYPE_SYMLINK:
			inode = flatfs_iget(sb, le64_to_cpu(ino) << FLATFS_INODE_BITS);

			inode_lock(inode);
			err = __flatfs_remove_file(sb, dir, inode, pi);
			inode_unlock(inode);

			if (err)
				return err;

			iput(inode); /* release the inode */
			break;
		default:
			printk(KERN_ERR "wrong type\n");
			break;
		}
    }
    brt_it_close(&it);

    /* TODO: Delayed free */
    flatfs_put_tree(&subtree);

	return err;
}

static int flatfs_rmdir_recur(struct inode *dir, struct dentry *dentry) {
    struct fentry *fe = (struct fentry *) dentry;
    struct inode *inode = fe->d_inode;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi = flatfs_get_inode(sb, inode->i_ino), *pidir;
    fastr_t path = fe->d_fullpath;
	flatfs_transaction_t *trans;
	int err = -ENOTEMPTY;

	FLATFS_INFO(FS_RMDIR_RECUR, "%.*s", FASTR_FMT(path));

	if (!inode)
		return -ENOENT;

	/* remove the key-value pairs and free all inodes recursively */
	err = __flatfs_rmdir_recur(sb, dom_under(fe), dir, path);
	if (err)
		goto err;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		err = PTR_ERR(trans);
		return err;
	}
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	/*inode->i_version++; */
	clear_nlink(inode);
	inode->i_ctime = dir->i_ctime;

	flatfs_memunlock_inode(sb, pi);
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	/* add the inode to truncate list in case a crash happens before the
	 * subsequent evict_inode is called. It will be deleted from the
	 * truncate list during evict_inode.
	 */
	flatfs_truncate_add(inode, inode->i_size);

	pidir = flatfs_get_inode(sb, dir->i_ino);
	flatfs_dec_count(dir, pidir);

	flatfs_commit_transaction(sb, trans);
err:
	return err;
}

static inline void cpdir_fix_dot_or_dotdot(brt_tree_t *tree, __le64 *valp, fastr_t path, struct inode *to_ino) {
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
    int ret = brt_get(tree, &qr, path);
    if (likely(!ret)) {
        *valp = qr.inode;
        brt_got(tree, &qr);
    } else {
        /* We step outside the subtree. */
        *valp = to_ino->i_ino >> FLATFS_INODE_BITS;
    }
    kfree(path.chars);
}

static int __flatfs_copy_symlink(brt_tree_t *tree, struct inode *dir, struct inode *src_inode, brt_qr_t *qr) {
    struct super_block *sb = tree->sbi->super;
	flatfs_transaction_t *trans;
	struct flatfs_inode *pi;
	struct inode *inode;
	int err = 0;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
    if (IS_ERR(trans)) {
        return PTR_ERR(trans);
    }

	inode = flatfs_new_inode(trans, dir, S_IFLNK|S_IRWXUGO);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		flatfs_abort_transaction(sb, trans);
		goto out;
	}

	inode->i_op = &flatfs_symlink_inode_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;

	pi = flatfs_get_inode(sb, inode->i_ino);
	pi->i_types = FLATFS_INODE_TYPE_SYMLINK;

	inode->i_size = src_inode->i_size;
	flatfs_update_isize(inode, pi);

	flatfs_inode_set_cow(sb, src_inode);

	/* copy file mappings */
	flatfs_copy_inode_subtree(src_inode, inode);

	unlock_new_inode(inode);

	/* update inode number */
	*qr->valp = inode->i_ino >> FLATFS_INODE_BITS;

	flatfs_commit_transaction(sb, trans);

out:
	return err;
}

static int __flatfs_copy_hardlink(brt_tree_t *tree, struct inode *dir, struct inode *src_inode, brt_qr_t *qr) {
    struct super_block *sb = tree->sbi->super;
	flatfs_transaction_t *trans;
	struct flatfs_inode *pi;
	struct inode *inode;
	int err = 0;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
    if (IS_ERR(trans)) {
        return PTR_ERR(trans);
    }

	/* In cp operation, the copied hard link becomes a copy of original linked file.
	 * The copied hard link has an independent inode and its data is same as original
	 * linked file. */

	inode = flatfs_new_inode(trans, dir, src_inode->i_mode);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		flatfs_abort_transaction(sb, trans);
		goto out;
	}

	inode->i_op = &flatfs_file_inode_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;
	inode->i_fop = &flatfs_xip_file_operations;

	inode->i_size = src_inode->i_size;
	inode->__i_nlink = 1;

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_FILE;
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_size = cpu_to_le64(inode->i_size);
	flatfs_memlock_inode(sb, pi);

	flatfs_inode_set_cow(sb, src_inode);

	/* copy file mappings */
	flatfs_copy_inode_subtree(src_inode, inode);

	unlock_new_inode(inode);

	/* update inode number */
    *qr->valp = inode->i_ino >> FLATFS_INODE_BITS;

	flatfs_commit_transaction(sb, trans);

out:
	return err;
}

static int __flatfs_copy_dir(brt_tree_t *tree, struct inode *dir, struct inode *src_inode, brt_qr_t *qr) {
    struct super_block *sb = tree->sbi->super;
	flatfs_transaction_t *trans;
	struct flatfs_inode *pi;
	struct inode *inode;
	int err = 0;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
    if (IS_ERR(trans)) {
        return PTR_ERR(trans);
    }

	/* create inode for this new directory */
	inode = flatfs_new_inode(trans, dir, S_IFDIR | src_inode->i_mode);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		flatfs_abort_transaction(sb, trans);
		goto out;
	}

	inode->i_op = &flatfs_dir_inode_operations;
    inode->i_flat_op = &flatfs_dir_inode_flat_operations;
	inode->i_fop = &flatfs_dir_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;

	inode->i_size = sb->s_blocksize;
	inode->__i_nlink = src_inode->__i_nlink;

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_DIR;
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_size = cpu_to_le64(inode->i_size);
	flatfs_memlock_inode(sb, pi);

	unlock_new_inode(inode);

	/* update inode number */
    *qr->valp = inode->i_ino >> FLATFS_INODE_BITS;

	flatfs_commit_transaction(sb, trans);

out:
	return err;
}

static int __flatfs_copy_file(brt_tree_t *tree, struct inode *dir, struct inode *src_inode, brt_qr_t *qr) {
    struct super_block *sb = tree->sbi->super;
	flatfs_transaction_t *trans;
	struct flatfs_inode *pi;
	struct inode *inode;
	int err = 0;

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
    if (IS_ERR(trans)) {
        return PTR_ERR(trans);
    }

	inode = flatfs_new_inode(trans, dir, src_inode->i_mode);
	if (IS_ERR(inode)) {
		err = PTR_ERR(inode);
		flatfs_abort_transaction(sb, trans);
		goto out;
	}

	inode->i_op = &flatfs_file_inode_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;
	inode->i_fop = &flatfs_xip_file_operations;

	inode->i_size = src_inode->i_size;
	/* The hard link does not preserve in default cp -r operation.
	 * If you want to preserve it, use cp --preserve=links instead.
	 * However, our cpdir operation does not support it currently. */
	inode->__i_nlink = 1;

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_FILE;
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_size = cpu_to_le64(inode->i_size);
	flatfs_memlock_inode(sb, pi);

	flatfs_inode_set_cow(sb, src_inode);

	/* copy file mappings */
	flatfs_copy_inode_subtree(src_inode, inode);

	unlock_new_inode(inode);

	/* update inode number */
    *qr->valp = inode->i_ino >> FLATFS_INODE_BITS;

	flatfs_commit_transaction(sb, trans);

out:
	return err;
}

static struct inode *__flatfs_copy_source_dir(flatfs_transaction_t *trans, struct super_block *sb,
                                              struct fentry *to, struct inode *dir, struct inode *src_inode) {
	struct flatfs_inode *pi;
	struct inode *inode;

	/* create inode for this new directory */
	inode = flatfs_new_inode(trans, dir, S_IFDIR | src_inode->i_mode);
	if (IS_ERR(inode)) {
		flatfs_abort_transaction(sb, trans);
		goto out;
	}

	inode->i_op = &flatfs_dir_inode_operations;
    inode->i_flat_op = &flatfs_dir_inode_flat_operations;
	inode->i_fop = &flatfs_dir_operations;
	inode->i_mapping->a_ops = &flatfs_aops_xip;

	inode->i_size = sb->s_blocksize;
	inode->__i_nlink = src_inode->__i_nlink;

	pi = flatfs_get_inode(sb, inode->i_ino);
	flatfs_memunlock_inode(sb, pi);
	pi->i_types = FLATFS_INODE_TYPE_DIR;
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_size = cpu_to_le64(inode->i_size);
	flatfs_memlock_inode(sb, pi);

	unlock_new_inode(inode);

	brt_add(tree_of(to), to->d_fullpath, inode->i_ino >> FLATFS_INODE_BITS, to->d_ppcs, 0);

    to->d_flags |= DCACHE_DIRECTORY_TYPE;
    to->d_inode = inode;

out:
	return inode;
}

static int __flatfs_cpdir_recur(struct flatfs_sb_info *sbi, brt_tree_t *subtree, struct inode *dir, struct inode *to_ino) {
	struct super_block *sb = sbi->super;
	struct flatfs_inode *pi;
	struct inode *src_inode;
	fastr_t path;
	struct list_head fix_dot_list_head;
	ino_t ino;
	int err = 0;
    brt_it_t it;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);

    INIT_LIST_HEAD(&fix_dot_list_head);

    brt_scan(subtree, &it, FASTR_LITERAL("\0"), FASTR_LITERAL("\x7f"));
    while (!brt_it_next(&it, &qr, 0)) {
        ino = qr.inode;
        path = qr.path;

		pi = flatfs_get_inode(sb, le64_to_cpu(ino) << FLATFS_INODE_BITS);
		src_inode = flatfs_iget(sb, le64_to_cpu(ino) << FLATFS_INODE_BITS);

		switch (pi->i_types) {
		case FLATFS_INODE_TYPE_DIR:
			if ((err = __flatfs_copy_dir(subtree, dir, src_inode, &qr)) < 0)
				goto out;
			break;
		case FLATFS_INODE_TYPE_HDLINK:
			if ((err = __flatfs_copy_hardlink(subtree, dir, src_inode, &qr)) < 0)
				goto out;
			break;
		case FLATFS_INODE_TYPE_SYMLINK:
			if ((err = __flatfs_copy_symlink(subtree, dir, src_inode, &qr)) < 0)
				goto out;
			break;
		case FLATFS_INODE_TYPE_FILE:
			if ((err = __flatfs_copy_file(subtree, dir, src_inode, &qr)) < 0)
				goto out;
			break;
		default:
			printk(KERN_ERR "wrong type\n");
			break;
		}

		iput(src_inode);
    }
    brt_it_close(&it);

out:
	return err;
}

static int flatfs_cpdir_recur(struct dentry *from, struct inode *to_dir, struct dentry *to) {
	struct super_block *sb = to_dir->i_sb;
    struct fentry *from_fe, *to_fe;
	flatfs_transaction_t *trans;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	brt_tree_t subtree, subtree_dup;
	struct flatfs_inode *pidir;
	fastr_t left, right;
	int err = 0;
    struct inode *to_ino;

    BUG_ON(!d_in_flat_ns(from));
    BUG_ON(!d_in_flat_ns(to));

    from_fe = (struct fentry *) from;
    to_fe = (struct fentry *) to;

	FLATFS_INFO(FS_CPDIR_RECUR, "copy %.*s to %.*s", FASTR_FMT(from_fe->d_fullpath), FASTR_FMT(to_fe->d_fullpath));

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		return PTR_ERR(trans);
	}

	/* 0. copy source directory to destination directory */
	to_ino = __flatfs_copy_source_dir(trans, sb, to_fe, to_dir, from->d_inode);
	if (IS_ERR(to_ino)) {
        err = PTR_ERR(to_ino);
        goto out;
    }

	/* increase directory nlinks */
	pidir = flatfs_get_inode(sb, to_dir->i_ino);
	flatfs_add_logentry(sb, trans, pidir, MAX_DATA_PER_LENTRY, LE_DATA);

	flatfs_inc_count(to_dir, pidir);

	flatfs_commit_transaction(sb, trans);

	/* 1. slice out the subtree */
    subtree = flatfs_get_tree(sb);
	left = dir_path_lowerbound(from_fe->d_fullpath);
	right = dir_path_upperbound(from_fe->d_fullpath);
	range_remove(sb, &subtree, tree_under(from_fe), left, right);
	kfree(left.chars);
	kfree(right.chars);

	/* 2. duplicate it */
    subtree_dup = flatfs_get_tree(sb);
	brt_dup(&subtree_dup, &subtree);

	/* 3. do batch name update */
    brt_chgpre(&subtree_dup, &(brt_chgpre_opt_t) {
            .src = from_fe->d_fullpath,
            .dst = to_fe->d_fullpath,
            .pppcs = to_fe->d_ppcs,
            .pppc = ino2pppc(to_ino)
    });

	/* 4. copy fs metadata */
	__flatfs_cpdir_recur(sbi, &subtree_dup, to_dir, to_ino);

    /* 5. insert two trees back to the original tree */
    range_insert(sb, tree_under(from_fe), &subtree);
    range_insert(sb, tree_under(to_fe), &subtree_dup);

    flatfs_put_tree(&subtree);
    flatfs_put_tree(&subtree_dup);

out:
	return err;
}

static int flatfs_rmdir(struct inode *dir, struct dentry *dentry)
{
    struct fentry *fe = (struct fentry *) dentry;
	struct super_block *sb = dir->i_sb;
	struct flatfs_inode *pi, *pidir;
    struct inode *inode;
	struct flatfs_sb_info* flatfs_sbi = FLATFS_SB(sb);
	flatfs_transaction_t *trans;
	int ret;

    BUG_ON(!d_in_flat_ns(dentry));

	FLATFS_INFO(FS_RMDIR, "%.*s", FASTR_FMT(fe->d_fullpath));
		
	if (!flatfs_empty_dir(fe)) {
        ret = -ENOTEMPTY;
		return ret;
	}

    ret = flatfs_namespace_remove(tree_of(fe), fe->d_fullpath);
	if (ret < 0) {
		ret = -ENOENT;
		goto out;
	}

    inode = dentry->d_inode;

    pi = flatfs_get_inode(sb, inode->i_ino);

	FLATFS_INFO(FS_RMDIR, "ino[%lu] count[%d] %.*s",
                inode->i_ino, atomic_read(&inode->i_count), FASTR_FMT(fe->d_fullpath));

	if (inode->i_nlink != 2)
		printk("flatfs_rmdir: empty directory %.*s ino %lu has nlink!=2 (%d)",
               FASTR_FMT(fe->d_fullpath), inode->i_ino, inode->i_nlink);

	FLATFS_INFO(FS_RMDIR, "bplus_tree_remove: %.*s", FASTR_FMT(fe->d_fullpath));

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES * 2 +
			MAX_DIRENTRY_LENTRIES);
	if (IS_ERR(trans)) {
        ret = PTR_ERR(trans);
		goto out;
	}
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	/*inode->i_version++; */
	clear_nlink(inode);
	inode->i_ctime = dir->i_ctime;

	flatfs_memunlock_inode(sb, pi);
	pi->i_links_count = cpu_to_le16(inode->i_nlink);
	pi->i_ctime = cpu_to_le32(inode->i_ctime.tv_sec);
	flatfs_memlock_inode(sb, pi);

	/* add the inode to truncate list in case a crash happens before the
	 * subsequent evict_inode is called. It will be deleted from the
	 * truncate list during evict_inode.
	 */
	flatfs_truncate_add(inode, inode->i_size);

	pidir = flatfs_get_inode(sb, dir->i_ino);
	flatfs_dec_count(dir, pidir);

	flatfs_commit_transaction(sb, trans);

out:
	return ret;
}

void flatfs_d_set_mounted(struct dentry *dentry) {
    struct super_block *sb;

    struct fentry *fe = (struct fentry *) dentry;
    brt_key_t left, right, rewalk_key;
    struct domain *dom, *new_dom;
    brt_tree_t *tree, *subtree;
    struct inode *inode;
    ppcs_t ppcs;

    BUG_ON(!d_in_flat_ns(dentry));

    BUG_ON(d_mountpoint(dentry));

    inode = fe->d_inode;
    dom = fe->d_domain;
    tree = dom->tree;

    sb = ((brt_tree_t *) dom->tree)->sbi->super;

    /* save dentry */
    flat_dget(dentry);
    inode->i_d_mp = dentry;

    /* slice out subtree under mntpoint */
    subtree = kmalloc(sizeof(brt_tree_t), GFP_ATOMIC);
    *subtree = flatfs_get_tree(sb);
    left = dir_path_lowerbound(fe->d_fullpath);
    right = dir_path_upperbound(fe->d_fullpath);
    range_remove(sb, subtree, tree, left, right);
    kfree(left.chars);
    kfree(right.chars);

    /* prepare PPCs (Note that @fe does not always have valid @d_ppcs.)  */
    flatfs_namespace_lookup(tree, fe->d_fullpath, &ppcs, NULL, 0);
    ppcs_shrink_suf(&ppcs, 1);

    /* insert rewalk key */
    rewalk_key = fastr(kmalloc(fe->d_fullpath.len + 2, GFP_ATOMIC), 0);
    fastr_append(&rewalk_key, fe->d_fullpath);
    fastr_append_ch(&rewalk_key, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append_ch(&rewalk_key, CTLCHR_PREFIX_INTERCEPTOR);
    flatfs_namespace_insert(tree, rewalk_key,
                            (inode->i_ino >> FLATFS_INODE_BITS) | INO_ANNOTATE_MNTPOINT, ppcs, ino2pppc(inode));
    kfree(rewalk_key.chars);

    /* annotate the original key */
    flatfs_namespace_update(tree, fe->d_fullpath, (inode->i_ino >> FLATFS_INODE_BITS) | INO_ANNOTATE_MNTPOINT);

    /* create new child domain */
    new_dom = kmalloc(sizeof(*new_dom) + fe->d_fullpath.len, GFP_ATOMIC);
    new_dom->tree = subtree;
    new_dom->parent = dom;
    new_dom->path = fastr((void *) (new_dom + 1), 0);
    fastr_append(&new_dom->path, fe->d_fullpath);
    INIT_LIST_HEAD(&new_dom->list);
    INIT_LIST_HEAD(&new_dom->children);
    list_add_tail(&new_dom->list, &dom->children);
    fe->d_domain = new_dom;

    FLATFS_INFO(VFS_MOUNT, "domain %lx (%.*s) created from %lx (%.*s)",
                (unsigned long) new_dom, FASTR_FMT(new_dom->path),
                (unsigned long) dom, FASTR_FMT(dom->path));
    FLATFS_INFO(VFS_MOUNT, "fentry %lx set as mntpoint %.*s", (unsigned long) fe, FASTR_FMT(fe->d_fullpath));
}

void flatfs_d_set_umounted(struct dentry *dentry) {
    struct super_block *sb;

    struct fentry *fe = (struct fentry *) dentry;
    struct domain *dom, *pdom;
    brt_tree_t *tree, *ptree;
    brt_key_t rewalk_key;
    struct inode *inode;

    BUG_ON(!d_in_flat_ns(dentry));

    inode = fe->d_inode;

    dom = fe->d_domain;
    tree = dom->tree;

    pdom = dom->parent;
    ptree = pdom->tree;

    sb = ((brt_tree_t *) dom->tree)->sbi->super;

    /* remove rewalk key in parent domain */
    rewalk_key = fastr(kmalloc(fe->d_fullpath.len + 2, GFP_ATOMIC), 0);
    fastr_append(&rewalk_key, fe->d_fullpath);
    fastr_append_ch(&rewalk_key, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append_ch(&rewalk_key, CTLCHR_PREFIX_INTERCEPTOR);
    flatfs_namespace_remove(ptree, rewalk_key);
    kfree(rewalk_key.chars);

    /* deannotate the original key in parent domain */
    flatfs_namespace_update(ptree, fe->d_fullpath, (inode->i_ino >> FLATFS_INODE_BITS) & ~INO_ANNOTATE_MNTPOINT);

    /* merge child domain to parent domain */
    range_insert(sb, ptree, tree);
    flatfs_put_tree(tree);

    /* remove the child domain */
    list_del(&dom->list);
    kfree(dom);

    inode->i_d_mp = NULL;
    flat_dput(dentry);

    FLATFS_INFO(VFS_MOUNT, "fentry %lx (%.*s) umounted", (unsigned long) fe, FASTR_FMT(fe->d_fullpath));
}

int use_range_rename_batch = 1;

static void range_rename_point(struct super_block *sb,
                               brt_tree_t *old_tree, brt_tree_t *new_tree,
                               struct fentry *old_fe, struct fentry *new_fe) {
    struct changelist {
        fastr_t from_path, to_path;
        ppcs_t to_ppcs;
        struct list_head list;
    };

    fastr_t old_path = old_fe->d_fullpath, new_path = new_fe->d_fullpath;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
    struct changelist *p, *tmp;
    LIST_HEAD(changelist);
    fastr_t left, right;
    brt_it_t it;
    int err;

    left = dir_path_lowerbound(old_path);
    right = dir_path_upperbound(old_path);

    brt_scan(old_tree, &it, left, FASTR_LITERAL("\x7f"));
    while (!(err = brt_it_next(&it, &qr, 0)) && fastr_strcmp(qr.path, right) <= 0) {
        struct changelist *r = kmalloc(sizeof(*r), GFP_ATOMIC);
        ppcs_t ppcs;
        void *buf;

        r->from_path = fastr(kmalloc(qr.path.len, GFP_ATOMIC), 0);
        flatfs_namespace_lookup(old_tree, r->from_path, &ppcs, NULL, 0);
        fastr_append(&r->from_path, qr.path);
        INIT_LIST_HEAD(&r->list);
        buf = ppcs.chars;

        r->to_path = fastr(kmalloc(qr.path.len - old_path.len + new_path.len, GFP_ATOMIC), 0);
        fastr_append(&r->to_path, new_path);
        fastr_append(&r->to_path, fastr_slice_after(qr.path, old_path.len));

        ppcs_shrink_pre(&ppcs, old_fe->d_depth);
        r->to_ppcs = ppcs_from_narr(kmalloc(new_fe->d_ppcs.len + ppcs.len, GFP_ATOMIC), 0);
        ppcs_append(&r->to_ppcs, new_fe->d_ppcs, 0, new_fe->d_depth);
        ppcs_append(&r->to_ppcs, ppcs, 0, ppcs_depth(ppcs));

        kfree(buf);

        FLATFS_INFO(FS_RENAME, "change from %.*s to %.*s", FASTR_FMT(r->from_path), FASTR_FMT(r->to_path));
    }
    BUG_ON(err);
    brt_it_close(&it);

    list_for_each_entry_safe(p, tmp, &changelist, list) {
        flatfs_namespace_insert(new_tree, p->to_path, qr.inode, p->to_ppcs, 0);
        flatfs_namespace_remove(new_tree, p->from_path);

        list_del(&p->list);
        kfree(p->to_ppcs.chars);
        kfree(p->to_path.chars);
        kfree(p->from_path.chars);
        kfree(p);
    }
}

static void range_rename_batch(struct super_block *sb,
                               brt_tree_t *old_tree, brt_tree_t *new_tree,
                               struct fentry *old_fe, struct fentry *new_fe) {
    fastr_t old_path = old_fe->d_fullpath, new_path = new_fe->d_fullpath;
    ppcs_t new_dir_ppc = new_fe->d_ppcs;
    void *dst_pppc_buffer;
    fastr_t left, right;
    brt_tree_t subtree;
    ppcs_t dst_pppc;

    subtree = flatfs_get_tree(sb);
    left = dir_path_lowerbound(old_path);
    right = dir_path_upperbound(old_path);
    range_remove(sb, &subtree, old_tree, left, right);
    kfree(left.chars);
    kfree(right.chars);

    FLATFS_INFO(FS_RENAME, "bplus_tree_remove_range: %.*s\n", FASTR_FMT(old_path));

    dst_pppc_buffer = kmalloc(new_dir_ppc.len, GFP_ATOMIC);
    dst_pppc = ppcs_from_narr(dst_pppc_buffer, 0);
    ppcs_append(&dst_pppc, new_dir_ppc, 0, ppcs_depth(new_dir_ppc));

    brt_chgpre(&subtree, &(brt_chgpre_opt_t) {
        .src = old_path,
        .dst = new_path,
        .pppcs = dst_pppc,
        .pppc = ino2pppc(old_fe->d_inode)
    });

    kfree(dst_pppc_buffer);

    FLATFS_INFO(FS_RENAME, "bplus_tree_update_range: %.*s --> %.*s", FASTR_FMT(old_path), FASTR_FMT(new_path));

    range_insert(sb, new_tree, &subtree);

    flatfs_put_tree(&subtree);

    FLATFS_INFO(FS_RENAME, "bplus_tree_insert_range: %.*s", FASTR_FMT(new_path));
}

static void range_rename(struct super_block *sb,
                         brt_tree_t *old_tree, brt_tree_t *new_tree,
                         struct fentry *old_fe, struct fentry *new_fe) {
    if (use_range_rename_batch) {
        range_rename_batch(sb, old_tree, new_tree, old_fe, new_fe);
    } else {
        range_rename_point(sb, old_tree, new_tree, old_fe, new_fe);
    }
}

static int flatfs_rename(struct inode *old_dir, struct dentry *old_dentry,
                         struct inode *new_dir, struct dentry *new_dentry,
			             unsigned int flags)
{
    struct fentry *old_fe = (struct fentry *) old_dentry;
    struct fentry *new_fe = (struct fentry *) new_dentry;
	struct inode* old_inode, *new_inode;
	flatfs_transaction_t *trans;
	struct super_block *sb = old_dir->i_sb;
	struct flatfs_inode *pi, *new_pidir, *old_pidir;
	brt_tree_t *old_tree, *new_tree;
	int err = -ENOENT;
    fastr_t old_path = old_fe->d_fullpath, new_path = new_fe->d_fullpath;
    ppcs_t new_dir_ppc = new_fe->d_ppcs;

	FLATFS_INFO(FS_RENAME, "flatfs_rename: rename %.*s to %.*s", FASTR_FMT(old_path), FASTR_FMT(new_path));

	trans = flatfs_new_transaction(sb, MAX_INODE_LENTRIES);
	if (IS_ERR(trans)) {
		return PTR_ERR(trans);
	}

	old_tree = tree_of(old_fe);
    new_tree = tree_of(new_fe);

	old_inode = old_dentry->d_inode;
	new_inode = new_dentry->d_inode;

	if (new_inode) {
		err = -ENOTEMPTY;
		/* rename a directory, dest directory must be empty */
		if (S_ISDIR(old_inode->i_mode) && !flatfs_empty_dir(new_fe))
			goto err;
	} else {
		if (S_ISDIR(old_inode->i_mode)) {
			err = -EMLINK;
			if (new_dir->i_nlink >= FLATFS_LINK_MAX)
				goto err;
		}
	}

	new_pidir = flatfs_get_inode(sb, new_dir->i_ino);

	pi = flatfs_get_inode(sb, old_inode->i_ino);
	flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);

	if (!S_ISDIR(old_inode->i_mode)) { /* 1. rename a file */
		if (new_inode) {
			/* (1) dest file exist */

			pi = flatfs_get_inode(sb, new_inode->i_ino);
			flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);
			new_inode->i_ctime = current_time(new_inode);

			flatfs_memunlock_inode(sb, pi);
			pi->i_ctime = cpu_to_le32(new_inode->i_ctime.tv_sec);
			if (new_inode->i_nlink)
				drop_nlink(new_inode);
			pi->i_links_count = cpu_to_le16(new_inode->i_nlink);
			flatfs_memlock_inode(sb, pi);

			if (!new_inode->i_nlink)
				flatfs_truncate_add(new_inode, new_inode->i_size);

			flatfs_add_logentry(sb, trans, new_pidir, MAX_DATA_PER_LENTRY, LE_DATA);
			/*new_dir->i_version++; */
			new_dir->i_ctime = new_dir->i_mtime = current_time(new_dir);
			flatfs_update_time(new_dir, new_pidir);

			flatfs_commit_transaction(sb, trans);

			/* remove old file */
			if (flatfs_namespace_remove(old_tree, old_path) < 0)
				goto out;

			FLATFS_INFO(FS_RENAME, "bplus_tree_remove: %.*s", FASTR_FMT(old_path));

			/* remove dest file */
			if (flatfs_namespace_remove(new_tree, new_path) < 0)
				goto out;

			FLATFS_INFO(FS_RENAME, "bplus_tree_remove: %.*s", FASTR_FMT(new_path));

			/* insert into new directory */
			if (flatfs_namespace_insert(new_tree, new_path, (old_inode->i_ino >> FLATFS_INODE_BITS),
                                        new_dir_ppc, 0))
				goto out;

			FLATFS_INFO(FS_RENAME, "bplus_tree_insert: %.*s inode[%lu]", FASTR_FMT(new_path), old_inode->i_ino);
		} else {
			/* (2) there are no dest file */
			/* remove old file */
			if (flatfs_namespace_remove(old_tree, old_path) < 0)
				goto out;

			FLATFS_INFO(FS_RENAME, "bplus_tree_remove: %.*s", FASTR_FMT(old_path));

			/* insert into new directory */
			if (flatfs_namespace_insert(new_tree, new_path, (old_inode->i_ino >> FLATFS_INODE_BITS),
                                        new_dir_ppc, 0))
				goto out;

			FLATFS_INFO(FS_RENAME, "bplus_tree_insert: %.*s inode[%lu]", FASTR_FMT(new_path), old_inode->i_ino);
		}
	} else { /* 2. rename a directory */
		flatfs_add_logentry(sb, trans, new_pidir, MAX_DATA_PER_LENTRY, LE_DATA);
		/*new_dir->i_version++; */
		new_dir->i_ctime = new_dir->i_mtime = current_time(new_dir);
		flatfs_update_time(new_dir, new_pidir);

		if (new_inode) {
			pi = flatfs_get_inode(sb, new_inode->i_ino);
			flatfs_add_logentry(sb, trans, pi, MAX_DATA_PER_LENTRY, LE_DATA);
			new_inode->i_ctime = current_time(new_inode);

			flatfs_memunlock_inode(sb, pi);
			if (new_inode->i_nlink) {
				drop_nlink(new_inode);
			}
			pi->i_ctime = cpu_to_le32(new_inode->i_ctime.tv_sec);
			if (new_inode->i_nlink)
				drop_nlink(new_inode);
			pi->i_links_count = cpu_to_le16(new_inode->i_nlink);
			flatfs_memlock_inode(sb, pi);

			if (!new_inode->i_nlink)
				flatfs_truncate_add(new_inode, new_inode->i_size);
		} else {
			flatfs_inc_count(new_dir, new_pidir);
			old_pidir = flatfs_get_inode(sb, old_dir->i_ino);
			flatfs_dec_count(old_dir, old_pidir);
		}

		flatfs_commit_transaction(sb, trans);

		/* remove old directory entry */
        err = flatfs_namespace_remove(old_tree, old_path);
        if (err) {
            goto out;
        }

        FLATFS_INFO(FS_RENAME, "bplus_tree_remove: %.*s", FASTR_FMT(old_path));

		if (!new_inode) {
			/* insert new directory entry if it does not exist */
			err = flatfs_namespace_insert(new_tree, new_path, old_inode->i_ino >> FLATFS_INODE_BITS,
                                          new_dir_ppc, 0);
			if (err)
            	goto out;
		}

        FLATFS_INFO(FS_RENAME, "bplus_tree_insert: %.*s -> %lu",
                    FASTR_FMT(new_path), old_inode->i_ino >> FLATFS_INODE_BITS);

        range_rename(sb, old_tree, new_tree, old_fe, new_fe);
	}

	return 0;
err:
	flatfs_abort_transaction(sb, trans);
out:
	return err;
}

struct super_block *fentry_sb(struct dentry *dentry) {
    struct fentry *fe = (struct fentry *) dentry;
    BUG_ON(!d_in_flat_ns(dentry));
    return ((brt_tree_t *) fe->d_domain->tree)->sbi->super;
}

const struct inode_operations flatfs_dir_inode_operations = {
	.create		= flatfs_create,
	.link		= flatfs_link,
	.unlink		= flatfs_unlink,
	.symlink	= flatfs_symlink,
	.mkdir		= flatfs_mkdir,
	.rmdir		= flatfs_rmdir,
    .rename		= flatfs_rename,
	.mknod		= flatfs_mknod,
	.getattr	= flatfs_getattr,
	.setattr	= flatfs_setattr,
	.get_acl	= NULL,
};

const struct inode_flat_operations flatfs_dir_inode_flat_operations = {
    .rmdir_recur = flatfs_rmdir_recur,
    .cpdir_recur = flatfs_cpdir_recur
};

const struct inode_operations flatfs_special_inode_operations = {
	.setattr	= flatfs_setattr,
	.get_acl	= NULL,
};
