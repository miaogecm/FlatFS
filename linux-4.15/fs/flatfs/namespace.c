/*
 * BRIEF DESCRIPTION
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
 * Namespace-related Operations
 *
 */
#include <linux/flatfs.h>
#include <linux/flatfs_define.h>

#include "flatfs.h"
#include "brtree/bag.h"
#include "brtree/tree.h"
#include "brtree/pslab.h"

extern int ppc_may_lookup(ppcs_t ppc);

/*
 * flatfs_namespace_insert: insert the entry in the namespace
 * @trans: transaction descriptor
 * @sbi: FlatFS super block descriptor
 * @key: inserted key
 * @data: inserted value
 */
long flatfs_namespace_insert(void *tree, brt_key_t key, brt_val_t data, ppcs_t pppcs, ppc_t pppc) {
	return brt_add(tree, key, data, pppcs, pppc);
}

int flatfs_namespace_update(void *tree, brt_key_t key, brt_val_t data) {
	return brt_upd(tree, key, data);
}

/*
 * flatfs_namespace_remove: remove the entry in the namespace
 * @trans: transaction descriptor
 * @sbi: FlatFS super block descriptor
 * @key: removed key
 */
int flatfs_namespace_remove(void *tree, brt_key_t key) {
	return brt_del(tree, key);
}

/* 
 * flatfs_namespace_lookup: find associated key in the namespace
 * @sbi: FlatFS super block descriptor
 * @path: searched file path
 * @get_ppc: 
 * @wd: rewalk data
 */
struct inode *
flatfs_namespace_lookup(void *tree, fastr_t path, ppcs_t *get_ppc, struct walk_desc *wd, int perm_check) {
    struct flatfs_sb_info *sbi = ((brt_tree_t *) tree)->sbi;
	struct inode *inode = NULL;
    struct fentry *fentry;
    bool got = false;
    brt_qr_t qr;
    long value;
    int ret;

    if (fastr_is_empty(path)) {
        fentry = (struct fentry *) sbi->super->s_root;
        qr.inode = fentry->d_inode->i_ino >> FLATFS_INODE_BITS;
        qr.path = FASTR_NULL;
        qr.ppcs = PPCS_NULL;
        goto got_qr;
    }

    qr.mode = wd ? BRT_QR_SEM : BRT_QR_RAW;
    if (get_ppc || perm_check) {
        qr.mode |= BRT_QR_PERM;
    }

    ret = brt_get(tree, &qr, path);
    if (ret == -ENOENT) {
        inode = ERR_PTR(ret);
        goto out1;
    }
    got = true;

got_qr:
    value = (long) qr.inode;
    if (unlikely(value == 0)) {
        /* /mnt/flatfs/.. */
        inode = ERR_PTR(-EDOTDOT);
        goto out2;
    }

	if (wd) {
        wd->skip = path.len - qr.path.len;
    	if (value & INO_ANNOTATE_SYMLINK) {
        	value &= ~INO_ANNOTATE_SYMLINK;
            wd->type = wd->skip ? WALK_MEET_SYMLINK : WALK_TRAILING_SYMLINK;
    	} else if (value & INO_ANNOTATE_MNTPOINT) {
        	value &= ~INO_ANNOTATE_MNTPOINT;
            wd->type = wd->skip ? WALK_MEET_MNTPOINT : WALK_TRAILING_MNTPOINT;
    	} else {
            BUG_ON(wd->skip);
            wd->type = WALK_NORMAL;
        }
	}

    inode = flatfs_iget(sbi->super, value << FLATFS_INODE_BITS);
    if (unlikely(IS_ERR(inode))) {
        goto out2;
    }

#if 0
    if (unlikely(perm_check && !ppc_may_lookup(qr.ppcs))) {
        iput(inode);
        inode = ERR_PTR(-EACCES);
    }
#endif

out2:
    if (get_ppc && likely(!IS_ERR(inode))) {
#if 0
        size_t n_ppc = ppcs_n_ppc(qr.ppcs);
        ppc_t ino_ppc = ino2pppc(inode);
        void *buf;

        if (!n_ppc || !ppc_may_merge(ppcs_get_ppc(qr.ppcs, n_ppc - 1), ino_ppc)) {
            n_ppc++;
        }

        buf = kmalloc(PPCS_SIZEOF(n_ppc), GFP_ATOMIC);
        *get_ppc = ppcs_from_narr(buf, 0);
        ppcs_append(get_ppc, qr.ppcs, 0, ppcs_depth(qr.ppcs));
        ppcs_append_ppc(get_ppc, ino_ppc);
#endif
        *get_ppc = PPCS_NULL;
    }

    if (got) {
        brt_got(tree, &qr);
    }

out1:
    return inode;
}

void flatfs_namespace_stat(struct flatfs_sb_info *sbi) {
	
}

/*
 * flatfs_namespace_init: initialize the namespace
 */
void flatfs_namespace_init(struct flatfs_sb_info* sbi, int mount_format) {
    brt_tree_t *tree = kmalloc(sizeof(brt_tree_t), GFP_ATOMIC);
    struct domain *root_domain;

    brbag_init();

	if (mount_format) {
        flatfs_oc_init(sbi->super);
        brt_init(sbi->super);
        flatfs_init_ptrees(sbi->super);
        *tree = flatfs_get_tree(sbi->super);
		flatfs_register_sysctl();
	} else {
        struct flatfs_super_block *flatfs_sb = flatfs_get_super(sbi->super);
        brt_pin(tree, &flatfs_sb->trees[0], sbi);
    }

    brt_set_main(tree);

    root_domain = kmalloc(sizeof(*root_domain), GFP_ATOMIC);
    root_domain->tree = tree;
    root_domain->path = FASTR_NULL;
    root_domain->parent = root_domain;
    INIT_LIST_HEAD(&root_domain->list);
    INIT_LIST_HEAD(&root_domain->children);
    sbi->root_domain = root_domain;

    INIT_LIST_HEAD(&sbi->leaf_list_head);

#ifdef CONFIG_FLATFS_DEBUG
	printk("flatfs namespace init: sbi %016lx tree %016lx\n", sbi, tree);
#endif
}

/*
 * flatfs_namespace_deinit: free the namespace
 */
void flatfs_namespace_deinit(struct flatfs_sb_info* sbi) {
    /* TODO: Tree deinit */
}
