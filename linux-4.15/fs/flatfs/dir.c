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
#include "brtree.h"

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

static inline char* entry_path(fastr_t pathname, char *buf, brt_qr_t *qr, size_t *len) {
    fastr_t dst = fastr(buf, 0);
    fastr_append(&dst, fastr_slice_after(qr->path, pathname.len));
    *len = dst.len;
    fastr_append_ch(&dst, '\0');
    return buf;
}

static inline char* child_entry_path(char *buf, brt_qr_t *qr, size_t *len) {
    size_t pos = fastr_find_last(qr->path, '/');
    fastr_t dst = fastr(buf, 0);
    fastr_append(&dst, fastr_slice_after(qr->path, pos + 1));
    *len = dst.len;
    fastr_append_ch(&dst, '\0');
    return buf;
}

static inline int is_shadow_entry_first(brt_qr_t *qr) {
    return !fastr_is_empty(qr->path) && qr->path.chars[qr->path.len - 1] == ASCII_FIRST;
}

static inline int is_shadow_entry_last(brt_qr_t *qr) {
    return !fastr_is_empty(qr->path) && qr->path.chars[qr->path.len - 1] == ASCII_LAST;
}

static inline int is_rewalk_entry(brt_qr_t *qr) {
    return !fastr_is_empty(qr->path) && qr->path.chars[qr->path.len - 1] == ASCII_REWALK;
}

static inline fastr_t copy_path(brt_qr_t *qr) {
    char *buffer = kmalloc(qr->path.len, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, qr->path);
    return fs;
}

static inline int reach_end(brt_qr_t *qr, fastr_t max) {
    return is_shadow_entry_last(qr) && !fastr_strcmp(qr->path, max);
}

/* @qr must be a left shadow ent. */
static inline fastr_t generate_skip_path(brt_qr_t *qr) {
    fastr_t fs = copy_path(qr);
    fs.chars[fs.len - 1] = ASCII_LAST;
    return fs;
}

static void update_ctx_pos_key(struct dir_context *ctx, brt_qr_t *qr) {
    ctx->pos_key = copy_path(qr);
}

static int flatfs_readdir(struct file *file, struct dir_context *ctx)
{
	struct inode *inode = file_inode(file);
    struct fentry *fe = (struct fentry *) file->f_path.dentry;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi;
	fastr_t pathname;
	brt_key_t min, max;
	char child_buf[1024], *child_str;
	timing_t readdir_time;
    brt_tree_t *tree = tree_under(fe);
    brt_it_t it;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
	size_t child_len;
	ino_t ino;
	unsigned int count = 0;
    int ret;

    BUG_ON(!d_in_flat_ns(file->f_path.dentry));

	pathname = fe->d_fullpath;
	if (pathname.len == 2 && pathname.chars[0] == '/' && pathname.chars[1] == '.') {
		pathname = FASTR_NULL;
	}

    FLATFS_INFO(FS_READDIR, "%.*s pos:%lld %.*s", FASTR_FMT(pathname), ctx->pos, FASTR_FMT(ctx->pos_key));

	min = FASTR_IS_NOT_NULL(ctx->pos_key) ? ctx->pos_key : dir_min_key(pathname);
	max = dir_max_key(pathname);

	FLATFS_START_TIMING(readdir_t, readdir_time);

    brt_lobnd(tree, &it, min);

    ret = brt_it_next(&it, &qr);

    if (reach_end(&qr, max)) {
		/* we reach the end in the last read directory operation, we should return 0 here */
		struct getdents_callback *buf = container_of(ctx, struct getdents_callback, ctx);
		buf->previous = NULL;
		buf->error = 0;
		goto out2;
    }

	if (!FASTR_IS_NOT_NULL(ctx->pos_key) && is_shadow_entry_first(&qr)) {
        ret = brt_it_next(&it, &qr);
        BUG_ON(ret);
    }

again:
	while (1) {
		if (unlikely(ret == -ENOENT || reach_end(&qr, max))) {
			break;
		}

		ino = qr.inode;

		switch (ino) {
		case SHADOW_DENTRY_INO:// shadow entry
			if (is_shadow_entry_first(&qr)) {
                kfree(min.chars);
				min = generate_skip_path(&qr);

				FLATFS_INFO(FS_READDIR, "skip directory %.*s %.*s", FASTR_FMT(qr.path), FASTR_FMT(min));

				/* adjust range */
                brt_it_close(&it);
                brt_lobnd(tree, &it, min);
                ret = brt_it_next(&it, &qr);
				goto again;
			}
			break;
			
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
		
			child_str = child_entry_path(child_buf, &qr, &child_len);
			pi = flatfs_get_inode(sb, ino << FLATFS_INODE_BITS);

			/* emit an entry */
			if (!dir_emit(ctx, child_str, child_len,
                          ino, IF2DT(le16_to_cpu(pi->i_mode)))) {
				goto out;
			}

			FLATFS_INFO(FS_READDIR, "emit an entry[%u] ino. %lu %s", ++count, ino, child_str);

			break;
		}

        ret = brt_it_next(&it, &qr);
	}

out:
	/* update ctx->pos_key, points to the next key */
	update_ctx_pos_key(ctx, &qr);

out2:
    brt_it_close(&it);

	FLATFS_END_TIMING(readdir_t, readdir_time);

	if (min.chars != ctx->pos_key.chars) {
        kfree(min.chars);
    }
	kfree(max.chars);
	
	return 0;
}

static int flatfs_readdir_recur(struct file *file, struct dir_context *ctx)
{
	struct inode *inode = file_inode(file);
    struct fentry *fe = (struct fentry *) file->f_path.dentry;
	struct super_block *sb = inode->i_sb;
	struct flatfs_inode *pi;
	fastr_t pathname;
	brt_key_t min, max;
	char child_buf[1024], *child_str;
	timing_t readdir_time;
    brt_tree_t *tree = tree_under(fe);
    brt_it_t it;
    DEFINE_BRT_QR(qr, BRT_QR_RAW);
	size_t child_len;
	ino_t ino;
#ifdef CONFIG_FLATFS_DEBUG
	unsigned int count = 0;
#endif
    int ret;

    BUG_ON(!d_in_flat_ns(file->f_path.dentry));

	pathname = fe->d_fullpath;
	if (pathname.len == 2 && pathname.chars[0] == '/' && pathname.chars[1] == '.') {
		pathname = FASTR_NULL;
	}

#ifdef CONFIG_FLATFS_DEBUG
	printk("flatfs_readdir: %.*s pos:%lld %.*s\n", FASTR_FMT(pathname), ctx->pos, FASTR_FMT(ctx->pos_key));
#endif

	min = FASTR_IS_NOT_NULL(ctx->pos_key) ? ctx->pos_key : dir_min_key(pathname);
	max = dir_max_key(pathname);

	FLATFS_START_TIMING(readdir_t, readdir_time);

    brt_lobnd(tree, &it, min);

    ret = brt_it_next(&it, &qr);

    if (reach_end(&qr, max)) {
		/* we reach the end in the last read directory operation, we should return 0 here */
		struct getdents_callback *buf = container_of(ctx, struct getdents_callback, ctx);
		buf->previous = NULL;
		buf->error = 0;
		goto out2;
    }

	if (!FASTR_IS_NOT_NULL(ctx->pos_key) && is_shadow_entry_first(&qr)) {
        ret = brt_it_next(&it, &qr);
        BUG_ON(ret);
    }

	while (1) {
		if (unlikely(ret == -ENOENT || reach_end(&qr, max))) {
			break;
		}

		ino = qr.inode;

		switch (ino) {
		case SHADOW_DENTRY_INO:// shadow entry
			break;

		case 0: // read FlatFS root directory and current entry is dotdot
			if (!dir_emit(ctx, "..", 3, ino, IF2DT(le16_to_cpu(S_IRWXUGO)))) {
				goto out;
			}
#ifdef CONFIG_FLATFS_DEBUG
			printk("emit an entry[%u] ino. %lu %s\n", ++count, ino, "..");
#endif
			break;

		default: // normal entries
            if (is_rewalk_entry(&qr)) {
    			break;
			}

			child_str = entry_path(pathname, child_buf, &qr, &child_len);
			pi = flatfs_get_inode(sb, ino << FLATFS_INODE_BITS);

			/* emit an entry */
			if (!dir_emit(ctx, child_str, child_len,
                          ino, IF2DT(le16_to_cpu(pi->i_mode)))) {
				goto out;
			}

#ifdef CONFIG_FLATFS_DEBUG
			printk("emit an entry[%u] ino. %lu %s\n", ++count, ino, child_str);
#endif
			break;
		}

        ret = brt_it_next(&it, &qr);
	}

out:
	/* update ctx->pos_key, points to the next key */
	update_ctx_pos_key(ctx, &qr);

out2:
    brt_it_close(&it);

	FLATFS_END_TIMING(readdir_t, readdir_time);

	if (min.chars != ctx->pos_key.chars) {
        kfree(min.chars);
    }
	kfree(max.chars);

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
