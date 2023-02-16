/*
 * BRIEF DESCRIPTION
 *
 * File operations for directories.
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
#include <linux/pagemap.h>
#include <linux/fastr.h>
#include <linux/pathman.h>

#include "flatfs.h"
#include "brtree/tree.h"

struct linux_dirent {
	unsigned long	d_ino;
	unsigned long	d_off;
	unsigned short	d_reclen;
	char		d_name[1];
};

struct getdents_callback {
	struct dir_context ctx;
	struct linux_dirent * current_dir;
	struct linux_dirent * previous;
	int count;
	int error;
};

/*
 *	Parent is locked.
 */

#define DT2IF(dt) (((dt) << 12) & S_IFMT)
#define IF2DT(sif) (((sif) & S_IFMT) >> 12)

static inline int is_rewalk_entry(brt_qr_t *qr) {
    return !fastr_is_empty(qr->path) && qr->path.chars[qr->path.len - 1] == CTLCHR_PREFIX_INTERCEPTOR;
}

static inline fastr_t generate_skip_path(fastr_t dir, brt_qr_t *qr) {
    char *buffer = kmalloc(dir.len + qr->path.len + 3, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, dir);
    fastr_append_ch(&fs, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append(&fs, qr->path);
    fastr_append_ch(&fs, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append_ch(&fs, '\x7f');
    return fs;
}

static inline fastr_t copy_path(fastr_t dir, brt_qr_t *qr) {
    char *buffer = kmalloc(dir.len + qr->path.len + 1, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, dir);
    fastr_append_ch(&fs, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append(&fs, qr->path);
    return fs;
}

static void update_ctx_pos_key(struct dir_context *ctx, fastr_t dir, brt_qr_t *qr) {
    ctx->pos_key = copy_path(dir, qr);
}

static int flatfs_readdir(struct file *file, struct dir_context *ctx)
{
	struct inode *inode = file_inode(file);
    struct fentry *fe = (struct fentry *) file->f_path.dentry;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi;
	timing_t readdir_time;
    brt_tree_t *tree = tree_under(fe);
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
    int ret, count = 0;
	brt_key_t min, max;
	fastr_t pathname;
    uint16_t mode;
    brt_it_t it;
	ino_t ino;

    BUG_ON(!d_in_flat_ns(file->f_path.dentry));

	pathname = fe->d_fullpath;

    FLATFS_INFO(FS_READDIR, "%.*s pos:%lld %.*s", FASTR_FMT(pathname), ctx->pos, FASTR_FMT(ctx->pos_key));

	min = FASTR_IS_NOT_NULL(ctx->pos_key) ? ctx->pos_key : dir_path_lowerbound(pathname);
	max = dir_path_upperbound(pathname);

	FLATFS_START_TIMING(readdir_t, readdir_time);

    brt_scan(tree, &it, min, max);

    FLATFS_INFO(FS_READDIR, "scan: [%.*s] to [%.*s]", FASTR_FMT(min), FASTR_FMT(max));

    ret = brt_it_next(&it, &qr, CTLCHR_COMPONENT_SEPARATOR);

    if (ret == -ENOENT) {
		/* we reach the end in the last read directory operation, we should return 0 here */
		struct getdents_callback *buf = container_of(ctx, struct getdents_callback, ctx);
		buf->previous = NULL;
		buf->error = 0;
		goto out2;
    }

	while (1) {
		if (unlikely(ret == -ENOENT)) {
			break;
		}

		ino = qr.inode;
        pi = flatfs_get_inode(sb, ino << FLATFS_INODE_BITS);
        mode = le16_to_cpu(pi->i_mode);

		switch (ino) {
		case 0: // read FlatFS root directory and current entry is dotdot	
			if (!dir_emit(ctx, "..", 3, ino, IF2DT(le16_to_cpu(S_IRWXUGO)))) {
				goto out;
			}

			FLATFS_INFO(FS_READDIR, "emit an entry[%u] ino. %lu %s", ++count, ino, "..");

			break;

		default: // normal entries
            if (is_rewalk_entry(&qr)) {
    			break;
			}

			/* emit an entry */
			if (!dir_emit(ctx, qr.path.chars, (int) qr.path.len, ino, IF2DT(mode))) {
				goto out;
			}

			FLATFS_INFO(FS_READDIR, "emit an entry[%u] ino. %lu %.*s", ++count, ino, FASTR_FMT(qr.path));

			break;
		}

        if (S_ISDIR(mode)) {
            kfree(min.chars);
            min = generate_skip_path(pathname, &qr);

            FLATFS_INFO(FS_READDIR, "skip directory %.*s %.*s", FASTR_FMT(qr.path), FASTR_FMT(min));

            /* skip subdirectory */
            brt_it_close(&it);
            brt_scan(tree, &it, min, max);
        }

        ret = brt_it_next(&it, &qr, CTLCHR_COMPONENT_SEPARATOR);
	}

out:
	/* update ctx->pos_key, points to the next key */
    if (ret == -ENOENT) {
        qr.path = FASTR_LITERAL("\x7f");
    }
	update_ctx_pos_key(ctx, pathname, &qr);

out2:
    brt_it_close(&it);

	FLATFS_END_TIMING(readdir_t, readdir_time);

	if (min.chars != ctx->pos_key.chars) {
        kfree(min.chars);
    }
    if (max.chars != qr.path.chars) {
        kfree(max.chars);
    }
	return 0;
}

static int flatfs_readdir_recur(struct file *file, struct dir_context *ctx)
{
	struct inode *inode = file_inode(file);
    struct fentry *fe = (struct fentry *) file->f_path.dentry;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi;
	timing_t readdir_time;
    brt_tree_t *tree = tree_under(fe);
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
    int ret, count = 0;
	brt_key_t min, max;
	fastr_t pathname;
    uint16_t mode;
    brt_it_t it;
	ino_t ino;

    BUG_ON(!d_in_flat_ns(file->f_path.dentry));

	pathname = fe->d_fullpath;

    FLATFS_INFO(FS_READDIR, "%.*s pos:%lld %.*s", FASTR_FMT(pathname), ctx->pos, FASTR_FMT(ctx->pos_key));

	min = FASTR_IS_NOT_NULL(ctx->pos_key) ? ctx->pos_key : dir_path_lowerbound(pathname);
	max = dir_path_upperbound(pathname);

	FLATFS_START_TIMING(readdir_t, readdir_time);

    brt_scan(tree, &it, min, max);

    FLATFS_INFO(FS_READDIR, "scan: [%.*s] to [%.*s]", FASTR_FMT(min), FASTR_FMT(max));

    ret = brt_it_next(&it, &qr, CTLCHR_COMPONENT_SEPARATOR);

    if (ret == -ENOENT) {
		/* we reach the end in the last read directory operation, we should return 0 here */
		struct getdents_callback *buf = container_of(ctx, struct getdents_callback, ctx);
		buf->previous = NULL;
		buf->error = 0;
		goto out2;
    }

	while (1) {
		if (unlikely(ret == -ENOENT)) {
			break;
		}

		ino = qr.inode;
        pi = flatfs_get_inode(sb, ino << FLATFS_INODE_BITS);
        mode = le16_to_cpu(pi->i_mode);

		switch (ino) {
		case 0: // read FlatFS root directory and current entry is dotdot
			if (!dir_emit(ctx, "..", 3, ino, IF2DT(le16_to_cpu(S_IRWXUGO)))) {
				goto out;
			}

			FLATFS_INFO(FS_READDIR, "emit an entry[%u] ino. %lu %s", ++count, ino, "..");

			break;

		default: // normal entries
            if (is_rewalk_entry(&qr)) {
    			break;
			}

			/* emit an entry */
			if (!dir_emit(ctx, qr.path.chars, (int) qr.path.len, ino, IF2DT(mode))) {
				goto out;
			}

			FLATFS_INFO(FS_READDIR, "emit an entry[%u] ino. %lu %.*s", ++count, ino, FASTR_FMT(qr.path));

			break;
		}

        ret = brt_it_next(&it, &qr, CTLCHR_COMPONENT_SEPARATOR);
	}

out:
	/* update ctx->pos_key, points to the next key */
    if (ret == -ENOENT) {
        qr.path = FASTR_LITERAL("\x7f");
    }
	update_ctx_pos_key(ctx, pathname, &qr);

out2:
    brt_it_close(&it);

	FLATFS_END_TIMING(readdir_t, readdir_time);

	if (min.chars != ctx->pos_key.chars) {
        kfree(min.chars);
    }
    if (max.chars != qr.path.chars) {
        kfree(max.chars);
    }
	return 0;
}

const struct file_operations flatfs_dir_operations = {
	.read			= generic_read_dir,
	.iterate_recur 	= flatfs_readdir_recur,
	.iterate		= flatfs_readdir,
	.fsync			= noop_fsync,
	.unlocked_ioctl = flatfs_ioctl,
#ifdef CONFIG_COMPAT
	.compat_ioctl	= flatfs_compat_ioctl,
#endif
};
