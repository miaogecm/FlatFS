/*
 * BRIEF DESCRIPTION
 *
 * Super block operations.
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
 
#include <linux/module.h>
#include <linux/string.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/parser.h>
#include <linux/vfs.h>
#include <linux/uaccess.h>
#include <linux/io.h>
#include <linux/seq_file.h>
#include <linux/mount.h>
#include <linux/mm.h>
#include <linux/ctype.h>
#include <linux/bitops.h>
#include <linux/magic.h>
#include <linux/exportfs.h>
#include <linux/random.h>
#include <linux/cred.h>
#include <linux/backing-dev.h>
#include <linux/list.h>
#include <linux/dax.h>
#include "flatfs.h"
#include "brtree/tree.h"

int measure_timing = 0;
int support_clwb = 0;
int support_pcommit = 0;

module_param(measure_timing, int, S_IRUGO);
MODULE_PARM_DESC(measure_timing, "Timing measurement");

static struct super_operations flatfs_sops;
static const struct export_operations flatfs_export_ops;
static struct kmem_cache *flatfs_inode_cachep;
static struct kmem_cache *flatfs_blocknode_cachep;
static struct kmem_cache *flatfs_transaction_cachep;
/* FIXME: should the following variable be one per FLATFS instance? */
unsigned int flatfs_dbgmask = 0;

#ifdef CONFIG_FLATFS_TEST
static void *first_flatfs_super;

struct flatfs_super_block *get_flatfs_super(void)
{
	return (struct flatfs_super_block *)first_flatfs_super;
}
EXPORT_SYMBOL(get_flatfs_super);
#endif

void flatfs_error_mng(struct super_block *sb, const char *fmt, ...)
{
	va_list args;

	printk("flatfs error: ");
	va_start(args, fmt);
	vprintk(fmt, args);
	va_end(args);

	if (test_opt(sb, ERRORS_PANIC))
		panic("flatfs: panic from previous error\n");
	if (test_opt(sb, ERRORS_RO)) {
		printk(KERN_CRIT "flatfs err: remounting filesystem read-only");
		sb->s_flags |= MS_RDONLY;
	}
}

static void flatfs_set_blocksize(struct super_block *sb, unsigned long size)
{
	int bits;

	/*
	 * We've already validated the user input and the value here must be
	 * between FLATFS_MAX_BLOCK_SIZE and FLATFS_MIN_BLOCK_SIZE
	 * and it must be a power of 2.
	 */
	bits = fls(size) - 1;
	sb->s_blocksize_bits = bits;
	sb->s_blocksize = (1 << bits);
}

static inline int flatfs_has_huge_ioremap(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = (struct flatfs_sb_info *)sb->s_fs_info;

	return sbi->s_mount_opt & FLATFS_MOUNT_HUGEIOREMAP;
}

static int flatfs_get_block_info(struct super_block *sb,
	struct flatfs_sb_info *sbi)
{
	struct dax_device *dax_dev;
	void *virt_addr = NULL;
	pfn_t __pfn_t;
	long size;
	int ret;

	ret = bdev_dax_supported(sb, PAGE_SIZE);
	if (ret) {
		flatfs_err(sb, "device does not support DAX\n");
		return -EINVAL;
	}

	sbi->s_bdev = sb->s_bdev;
	dax_dev = fs_dax_get_by_host(sb->s_bdev->bd_disk->disk_name);
	if (!dax_dev) {
		flatfs_err(sb, "Couldn't retrieve DAX device\n");
		return -EINVAL;
	}

	size = dax_direct_access(dax_dev, 0, LONG_MAX / PAGE_SIZE,
				&virt_addr, &__pfn_t) * PAGE_SIZE;
	if (size <= 0) {
		flatfs_err(sb, "direct_access failed\n");
		return -EINVAL;
	}

	sbi->virt_addr = virt_addr;
	sbi->phys_addr = pfn_t_to_pfn(__pfn_t) << PAGE_SHIFT;
	sbi->initsize = size;

	printk("persistent memory-mapped addr: [%016lx, %016lx]\n", 
		sbi->virt_addr, (unsigned long)sbi->virt_addr + size);
	
	return 0;
}

static loff_t flatfs_max_size(int bits)
{
	loff_t res;

	res = (1ULL << (3 * 9 + bits)) - 1;

	if (res > MAX_LFS_FILESIZE)
		res = MAX_LFS_FILESIZE;

	flatfs_dbg_verbose("max file size %llu bytes\n", res);
	return res;
}

enum {
	Opt_bpi, Opt_init, Opt_jsize,
	Opt_num_inodes, Opt_mode, Opt_uid,
	Opt_gid, Opt_blocksize, Opt_wprotect, Opt_wprotectold,
	Opt_err_cont, Opt_err_panic, Opt_err_ro,
	Opt_hugemmap, Opt_nohugeioremap, Opt_dbgmask, Opt_bs, Opt_err
};

static const match_table_t tokens = {
	{ Opt_bpi,	     "bpi=%u"		  },
	{ Opt_init,	     "init"		  },
	{ Opt_jsize,     "jsize=%s"		  },
	{ Opt_num_inodes,"num_inodes=%u"  },
	{ Opt_mode,	     "mode=%o"		  },
	{ Opt_uid,	     "uid=%u"		  },
	{ Opt_gid,	     "gid=%u"		  },
	{ Opt_wprotect,	     "wprotect"		  },
	{ Opt_wprotectold,   "wprotectold"	  },
	{ Opt_err_cont,	     "errors=continue"	  },
	{ Opt_err_panic,     "errors=panic"	  },
	{ Opt_err_ro,	     "errors=remount-ro"  },
	{ Opt_hugemmap,	     "hugemmap"		  },
	{ Opt_nohugeioremap, "nohugeioremap"	  },
	{ Opt_dbgmask,	     "dbgmask=%u"	  },
	{ Opt_bs,	     "backing_dev=%s"	  },
	{ Opt_err,	     NULL		  },
};

static int flatfs_parse_options(char *options, struct flatfs_sb_info *sbi,
			       bool remount)
{
	char *p, *rest;
	substring_t args[MAX_OPT_ARGS];
	int option;

	if (!options)
		return 0;

	while ((p = strsep(&options, ",")) != NULL) {
		int token;
		if (!*p)
			continue;

		token = match_token(p, tokens, args);
		switch (token) {
		case Opt_bpi:
			if (remount)
				goto bad_opt;
			if (match_int(&args[0], &option))
				goto bad_val;
			sbi->bpi = option;
			break;
		case Opt_uid:
			if (remount)
				goto bad_opt;
			if (match_int(&args[0], &option))
				goto bad_val;
			sbi->uid = make_kuid(current_user_ns(), option);
			break;
		case Opt_gid:
			if (match_int(&args[0], &option))
				goto bad_val;
			sbi->gid = make_kgid(current_user_ns(), option);
			break;
		case Opt_mode:
			if (match_octal(&args[0], &option))
				goto bad_val;
			sbi->mode = option & 01777U;
			break;
		case Opt_init:
			if (remount)
				goto bad_opt;
			set_opt(sbi->s_mount_opt, FORMAT);
			break;
		case Opt_jsize:
			if (remount)
				goto bad_opt;
			/* memparse() will accept a K/M/G without a digit */
			if (!isdigit(*args[0].from))
				goto bad_val;
			sbi->jsize = memparse(args[0].from, &rest);
			/* make sure journal size is integer power of 2 */
			if (sbi->jsize & (sbi->jsize - 1) ||
				sbi->jsize < FLATFS_MINIMUM_JOURNAL_SIZE) {
				flatfs_dbg("Invalid jsize: "
					"must be whole power of 2 & >= 64KB\n");
				goto bad_val;
			}
			break;
		case Opt_num_inodes:
			if (remount)
				goto bad_opt;
			if (match_int(&args[0], &option))
				goto bad_val;
			sbi->num_inodes = option;
			break;
		case Opt_err_panic:
			clear_opt(sbi->s_mount_opt, ERRORS_CONT);
			clear_opt(sbi->s_mount_opt, ERRORS_RO);
			set_opt(sbi->s_mount_opt, ERRORS_PANIC);
			break;
		case Opt_err_ro:
			clear_opt(sbi->s_mount_opt, ERRORS_CONT);
			clear_opt(sbi->s_mount_opt, ERRORS_PANIC);
			set_opt(sbi->s_mount_opt, ERRORS_RO);
			break;
		case Opt_err_cont:
			clear_opt(sbi->s_mount_opt, ERRORS_RO);
			clear_opt(sbi->s_mount_opt, ERRORS_PANIC);
			set_opt(sbi->s_mount_opt, ERRORS_CONT);
			break;
		case Opt_wprotect:
			if (remount)
				goto bad_opt;
			set_opt(sbi->s_mount_opt, PROTECT);
			flatfs_info
				("FLATFS: Enabling new Write Protection (CR0.WP)\n");
			break;
		case Opt_wprotectold:
			if (remount)
				goto bad_opt;
			set_opt(sbi->s_mount_opt, PROTECT_OLD);
			flatfs_info
				("FLATFS: Enabling old Write Protection (PAGE RW Bit)\n");
			break;
		case Opt_hugemmap:
			if (remount)
				goto bad_opt;
			set_opt(sbi->s_mount_opt, HUGEMMAP);
			flatfs_info("FLATFS: Enabling huge mappings for mmap\n");
			break;
		case Opt_nohugeioremap:
			if (remount)
				goto bad_opt;
			clear_opt(sbi->s_mount_opt, HUGEIOREMAP);
			flatfs_info("FLATFS: Disabling huge ioremap\n");
			break;
		case Opt_dbgmask:
			if (match_int(&args[0], &option))
				goto bad_val;
			flatfs_dbgmask = option;
			break;
		default: {
			goto bad_opt;
		}
		}
	}

	return 0;

bad_val:
	printk(KERN_INFO "Bad value '%s' for mount option '%s'\n", args[0].from,
	       p);
	return -EINVAL;
bad_opt:
	printk(KERN_INFO "Bad mount option: \"%s\"\n", p);
	return -EINVAL;
}

static bool flatfs_check_size (struct super_block *sb, unsigned long size)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	unsigned long minimum_size, num_blocks;

	/* space required for super block and root directory */
	minimum_size = 2 << sb->s_blocksize_bits;

	/* space required for inode table */
	if (sbi->num_inodes > 0)
		num_blocks = (sbi->num_inodes >>
			(sb->s_blocksize_bits - FLATFS_INODE_BITS)) + 1;
	else
		num_blocks = 1;
	minimum_size += (num_blocks << sb->s_blocksize_bits);
	/* space required for journal */
	minimum_size += sbi->jsize;

	if (size < minimum_size)
	    return false;

	return true;
}


/*
 * FlatFS Storage Layout:
 *
 * | Super Block | Journal Metadata | Inode Table[0] | Inode Table[1] | ... | Inode Table[n] | Backup Super Block | Unused Space | Journal Data | Data Area |
 * 
 * FLATFS_SB_SIZE = super block + Journal Metadata + Inode Table[0-n]
 */
static struct flatfs_inode *flatfs_init(struct super_block *sb,
				      unsigned long size)
{
	unsigned long blocksize;
	u64 journal_meta_start, journal_data_start, inode_table_start;
	struct flatfs_inode *root_i;
	struct flatfs_super_block *super;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	unsigned long blocknr;
	int cpu;
	u64 start_addr;

	flatfs_info("creating an empty flatfs of size %lu\n", size);
	sbi->block_start = (unsigned long)0;
	sbi->block_end = ((unsigned long)(size) >> PAGE_SHIFT);
	sbi->num_free_blocks = ((unsigned long)(size) >> PAGE_SHIFT);

	if (!sbi->virt_addr) {
		printk(KERN_ERR "ioremap of the flatfs image failed(1)\n");
		return ERR_PTR(-EINVAL);
	}
#ifdef CONFIG_FLATFS_TEST
	if (!first_flatfs_super)
		first_flatfs_super = sbi->virt_addr;
#endif

	flatfs_dbg_verbose("flatfs: Default block size set to 4K\n");
	blocksize = sbi->blocksize = FLATFS_DEF_BLOCK_SIZE_4K;

	flatfs_set_blocksize(sb, blocksize);
	blocksize = sb->s_blocksize;

	if (sbi->blocksize && sbi->blocksize != blocksize)
		sbi->blocksize = blocksize;

	if (!flatfs_check_size(sb, size)) {
		flatfs_dbg("Specified FLATFS size too small 0x%lx. Either increase"
			" FLATFS size, or reduce num. of inodes (minimum 32)" 
			" or journal size (minimum 64KB)\n", size);
		return ERR_PTR(-EINVAL);
	}

	journal_meta_start = sizeof(struct flatfs_super_block);
	journal_meta_start = (journal_meta_start + CACHELINE_SIZE - 1) &
		~(CACHELINE_SIZE - 1);
	inode_table_start = journal_meta_start + sizeof(flatfs_journal_t);
	inode_table_start = (inode_table_start + CACHELINE_SIZE - 1) &
		~(CACHELINE_SIZE - 1);

	if ((inode_table_start + sizeof(struct flatfs_inode)) > FLATFS_SB_SIZE) {
		flatfs_dbg("FLATFS super block defined too small. defined 0x%x, "
				"required 0x%llx\n", FLATFS_SB_SIZE,
			inode_table_start + sizeof(struct flatfs_inode));
		return ERR_PTR(-EINVAL);
	}

	journal_data_start = 2 * FLATFS_SB_SIZE;
	journal_data_start = (journal_data_start + blocksize - 1) &
		~(blocksize - 1);

	flatfs_dbg_verbose("journal meta start %llx data start 0x%llx, "
		"journal size 0x%x, inode_table 0x%llx\n", journal_meta_start,
		journal_data_start, sbi->jsize, inode_table_start);
	flatfs_dbg_verbose("max file name len %d\n", (unsigned int)FLATFS_NAME_LEN);

	super = flatfs_get_super(sb);
	flatfs_memunlock_range(sb, super, journal_data_start);

	/* clear out super-block and inode table */
	memset_nt(super, 0, journal_data_start);
	super->s_size = cpu_to_le64(size);
	super->s_blocksize = cpu_to_le32(blocksize);
	super->s_magic = cpu_to_le16(FLATFS_SUPER_MAGIC);
	super->s_journal_offset = cpu_to_le64(journal_meta_start);
	
	for (cpu = 0; cpu < sbi->num_cpus; cpu ++) {
		super->s_inode_table_offset[cpu] = 
			cpu_to_le64(inode_table_start + FLATFS_INODE_SIZE * cpu);
	}

	flatfs_init_blockmap(sb, journal_data_start + sbi->jsize);

	flatfs_memlock_range(sb, super, journal_data_start);

	if (flatfs_journal_hard_init(sb, journal_data_start, sbi->jsize) < 0) {
		printk(KERN_ERR "Journal hard initialization failed\n");
		return ERR_PTR(-EINVAL);
	}

	if (flatfs_init_inode_table(sb) < 0)
		return ERR_PTR(-EINVAL);

	flatfs_memunlock_range(sb, super, FLATFS_SB_SIZE*2);
	flatfs_sync_super(super);
	flatfs_memlock_range(sb, super, FLATFS_SB_SIZE*2);

	flatfs_flush_buffer(super, FLATFS_SB_SIZE, false);
	flatfs_flush_buffer((char *)super + FLATFS_SB_SIZE, sizeof(*super), false);

	flatfs_new_block(sb, &blocknr, FLATFS_BLOCK_TYPE_4K, 1);

	root_i = flatfs_get_inode(sb, FLATFS_ROOT_INO);

	flatfs_memunlock_inode(sb, root_i);
	root_i->i_mode = cpu_to_le16(sbi->mode | S_IFDIR);
	root_i->i_uid = cpu_to_le32(from_kuid(&init_user_ns, sbi->uid));
	root_i->i_gid = cpu_to_le32(from_kgid(&init_user_ns, sbi->gid));
	root_i->i_links_count = cpu_to_le16(2);
	root_i->i_blk_type = FLATFS_BLOCK_TYPE_4K;
	root_i->i_flags = 0;
	root_i->i_blocks = cpu_to_le64(1);
	root_i->i_size = cpu_to_le64(sb->s_blocksize);
	root_i->i_atime = root_i->i_mtime = root_i->i_ctime =
		cpu_to_le32(get_seconds());
	root_i->root = cpu_to_le64(flatfs_get_block_off(sb, blocknr,
						       FLATFS_BLOCK_TYPE_4K));
	root_i->height = 0;
	root_i->i_types = FLATFS_INODE_TYPE_DIR;
	/* flatfs_sync_inode(root_i); */
	flatfs_memlock_inode(sb, root_i);
	flatfs_flush_buffer(root_i, sizeof(*root_i), false);

	/* Init flatfs namespace */
	flatfs_namespace_init(sbi, 1);

	start_addr = (u64) super;
	printk("flatfs: Memory layout:\n");
	printk("flatfs: flatfs_super_block: [%016llx, %016llx)\n", start_addr, start_addr + journal_meta_start);
	printk("flatfs: journal metadata: [%016llx, %016llx)\n", start_addr + journal_meta_start, start_addr + inode_table_start);
	printk("flatfs: per-core inode table:\n");
    for (cpu = 0; cpu < sbi->num_cpus; cpu ++) {
        printk("flatfs: cpu[%d] inode table: [%016llx, %016llx)", cpu, start_addr + super->s_inode_table_offset[cpu],
                                                                start_addr + super->s_inode_table_offset[cpu] + FLATFS_INODE_SIZE);
        printk(" ino. [%u %u] total [%u] free [%u] hint [%u]\n",
               sbi->itable[cpu].s_free_inode_hint,
               sbi->itable[cpu].s_free_inode_hint + sbi->itable[cpu].s_inodes_count - FLATFS_FREE_INODE_HINT_START,
               sbi->itable[cpu].s_inodes_count,
               sbi->itable[cpu].s_free_inodes_count,
               sbi->itable[cpu].s_free_inode_hint);
    }
    printk("flatfs: journal data: [%016llx, %016llx)\n", start_addr + journal_data_start, start_addr + journal_data_start + sbi->jsize);
	
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	return root_i;
}

static int flatfs_init_root_directory(struct super_block *sb, struct inode* root_i) {
    return 0;
}

static inline void set_default_opts(struct flatfs_sb_info *sbi)
{
	/* set_opt(sbi->s_mount_opt, PROTECT); */
	set_opt(sbi->s_mount_opt, HUGEIOREMAP);
	set_opt(sbi->s_mount_opt, ERRORS_CONT);
	sbi->jsize = FLATFS_DEFAULT_JOURNAL_SIZE;
}

static void flatfs_root_check(struct super_block *sb, struct flatfs_inode *root_pi)
{
	if (!S_ISDIR(le16_to_cpu(root_pi->i_mode)))
		flatfs_warn("root is not a directory!\n");
#if 0
	if (flatfs_calc_checksum((u8 *)root_pi, FLATFS_INODE_SIZE)) {
		flatfs_dbg("checksum error in root inode, trying to fix\n");
		goto fail3;
	}
#endif
}

int flatfs_check_integrity(struct super_block *sb,
			  struct flatfs_super_block *super)
{
	struct flatfs_super_block *super_redund;

	super_redund =
		(struct flatfs_super_block *)((char *)super + FLATFS_SB_SIZE);

	/* Do sanity checks on the superblock */
	if (le16_to_cpu(super->s_magic) != FLATFS_SUPER_MAGIC) {
		if (le16_to_cpu(super_redund->s_magic) != FLATFS_SUPER_MAGIC) {
			printk(KERN_ERR "Can't find a valid flatfs partition\n");
			goto out;
		} else {
			flatfs_warn
				("Error in super block: try to repair it with "
				"the redundant copy");
			/* Try to auto-recover the super block */
			if (sb)
				flatfs_memunlock_super(sb, super);
			memcpy(super, super_redund,
				sizeof(struct flatfs_super_block));
			if (sb)
				flatfs_memlock_super(sb, super);
			flatfs_flush_buffer(super, sizeof(*super), false);
			flatfs_flush_buffer((char *)super + FLATFS_SB_SIZE,
				sizeof(*super), false);

		}
	}

	/* Read the superblock */
	if (flatfs_calc_checksum((u8 *)super, FLATFS_SB_STATIC_SIZE(super))) {
		if (flatfs_calc_checksum((u8 *)super_redund,
					FLATFS_SB_STATIC_SIZE(super_redund))) {
			printk(KERN_ERR "checksum error in super block\n");
			goto out;
		} else {
			flatfs_warn
				("Error in super block: try to repair it with "
				"the redundant copy");
			/* Try to auto-recover the super block */
			if (sb)
				flatfs_memunlock_super(sb, super);
			memcpy(super, super_redund,
				sizeof(struct flatfs_super_block));
			if (sb)
				flatfs_memlock_super(sb, super);
			flatfs_flush_buffer(super, sizeof(*super), false);
			flatfs_flush_buffer((char *)super + FLATFS_SB_SIZE,
				sizeof(*super), false);
		}
	}

	return 1;
out:
	return 0;
}

static void flatfs_recover_truncate_list(struct super_block *sb)
{
	struct flatfs_inode_truncate_item *head = flatfs_get_truncate_list_head(sb);
	u64 ino_next = le64_to_cpu(head->i_next_truncate);
	struct flatfs_inode *pi;
	struct flatfs_inode_truncate_item *li;
	struct inode *inode;

	if (ino_next == 0)
		return;

	while (ino_next != 0) {
		pi = flatfs_get_inode(sb, ino_next);
		li = (struct flatfs_inode_truncate_item *)(pi + 1);
		inode = flatfs_iget(sb, ino_next);
		if (IS_ERR(inode))
			break;
		flatfs_dbg("Recover ino %llx nlink %d sz %llx:%llx\n", ino_next,
			inode->i_nlink, pi->i_size, li->i_truncatesize);
		if (inode->i_nlink) {
			/* set allocation hint */
			flatfs_set_blocksize_hint(sb, pi, 
					le64_to_cpu(li->i_truncatesize));
			flatfs_setsize(inode, le64_to_cpu(li->i_truncatesize));
			flatfs_update_isize(inode, pi);
		} else {
			/* free the inode */
			flatfs_dbg("deleting unreferenced inode %lx\n",
				inode->i_ino);
		}
		iput(inode);
		flatfs_flush_buffer(pi, CACHELINE_SIZE, false);
		ino_next = le64_to_cpu(li->i_next_truncate);
	}
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	/* reset the truncate_list */
	flatfs_memunlock_range(sb, head, sizeof(*head));
	head->i_next_truncate = 0;
	flatfs_memlock_range(sb, head, sizeof(*head));
	flatfs_flush_buffer(head, sizeof(*head), false);
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
}

struct fentry_root {
    struct fentry b;
    /* VFS needs to know @d_sb in root dentry... Very dirty trick here. :( */
    char padding[offsetof(struct dentry, d_sb) - sizeof(struct fentry)];
    struct super_block *d_sb;
	struct lockref d_lockref;	/* per-dentry lock and refcount */
};

static struct dentry *make_flatfs_root(struct inode *inode, struct super_block *sb) {
    struct flatfs_sb_info *sbi = FLATFS_SB(sb);

    struct fentry_root *root = kmalloc(sizeof(*root), GFP_ATOMIC);
    struct fentry *fentry = (struct fentry *) root;
    struct dentry *dentry = (struct dentry *) root;

    fentry->d_flags = DCACHE_FLAT | DCACHE_DIRECTORY_TYPE;
    fentry->d_fullpath = FASTR_NULL;
    fentry->d_ppcs = FASTR_NULL;
    fentry->d_depth = 0;
    fentry->d_refcnt = 1;
    fentry->d_parent = dentry;
    fentry->d_inode = inode;
    fentry->d_domain = sbi->root_domain;
    seqcount_init(&fentry->d_seq);

    dentry->d_sb = sb;
    dentry->d_lockref.count = 1;
    spin_lock_init(&dentry->d_lock);

    return dentry;
}

static int flatfs_fill_super(struct super_block *sb, void *data, int silent)
{
	struct flatfs_super_block *super;
	struct flatfs_inode *root_pi;
	struct flatfs_sb_info *sbi = NULL;
	struct inode *root_i = NULL;
	unsigned long blocksize;
	u32 random = 0;
	int retval = -EINVAL, cpu;

	BUILD_BUG_ON(sizeof(struct flatfs_super_block) > FLATFS_SB_SIZE);
	BUILD_BUG_ON(sizeof(struct flatfs_inode) > FLATFS_INODE_SIZE);

	if (arch_has_pcommit()) {
		flatfs_info("arch has PCOMMIT support\n");
		support_pcommit = 1;
	} else {
		flatfs_info("arch does not have PCOMMIT support\n");
	}

	if (arch_has_clwb()) {
		flatfs_info("arch has CLWB support\n");
		support_clwb = 1;
	} else {
		flatfs_info("arch does not have CLWB support\n");
	}

	sbi = kzalloc(sizeof(struct flatfs_sb_info), GFP_KERNEL);
	if (!sbi)
		return -ENOMEM;
	sb->s_fs_info = sbi;
	sbi->super = sb;

	set_default_opts(sbi);

	if (flatfs_get_block_info(sb, sbi))
		goto out;

	get_random_bytes(&random, sizeof(u32));
	atomic_set(&sbi->next_generation, random);

	sbi->num_cpus = num_online_cpus();

	/* Init with default values */
	INIT_LIST_HEAD(&sbi->block_inuse_head);
	sbi->mode = (S_IRUGO | S_IXUGO | S_IWUSR);
	sbi->uid = current_fsuid();
	sbi->gid = current_fsgid();
	set_opt(sbi->s_mount_opt, XIP);
	clear_opt(sbi->s_mount_opt, PROTECT);
	set_opt(sbi->s_mount_opt, HUGEIOREMAP);

	INIT_LIST_HEAD(&sbi->s_truncate);
	mutex_init(&sbi->s_truncate_lock);
	for (cpu = 0; cpu < sbi->num_cpus; cpu ++) {
		mutex_init(&sbi->itable[cpu].inode_table_mutex);
	}
	
	mutex_init(&sbi->s_lock);

	if (flatfs_parse_options(data, sbi, 0))
		goto out;

	set_opt(sbi->s_mount_opt, MOUNTING);

	flatfs_init_blocks(sbi);

	/* Init a new flatfs instance */
	if (sbi->s_mount_opt & FLATFS_MOUNT_FORMAT) {
		root_pi = flatfs_init(sb, sbi->initsize);
		if (IS_ERR(root_pi))
			goto out;
		super = flatfs_get_super(sb);
		goto setup_sb;
	}
	flatfs_dbg_verbose("checking physical address 0x%016llx for flatfs image\n",
		  (u64)sbi->phys_addr);

	super = flatfs_get_super(sb);

	if (flatfs_journal_soft_init(sb)) {
		retval = -EINVAL;
		printk(KERN_ERR "Journal initialization failed\n");
		goto out;
	}
	if (flatfs_recover_journal(sb)) {
		retval = -EINVAL;
		printk(KERN_ERR "Journal recovery failed\n");
		goto out;
	}

	if (flatfs_check_integrity(sb, super) == 0) {
		flatfs_dbg("Memory contains invalid flatfs %x:%x\n",
				le16_to_cpu(super->s_magic), FLATFS_SUPER_MAGIC);
		goto out;
	}

	blocksize = le32_to_cpu(super->s_blocksize);
	flatfs_set_blocksize(sb, blocksize);

	flatfs_dbg_verbose("blocksize %lu\n", blocksize);

	/* Read the root inode */
	root_pi = flatfs_get_inode(sb, FLATFS_ROOT_INO);

	/* Check that the root inode is in a sane state */
	flatfs_root_check(sb, root_pi);

	//flatfs_namespace_init(sbi, 0);

#ifdef CONFIG_FLATFS_TEST
	if (!first_flatfs_super)
		first_flatfs_super = sbi->virt_addr;
#endif

	/* Set it all up.. */
setup_sb:
	sb->s_magic = le16_to_cpu(super->s_magic);
	sb->s_op = &flatfs_sops;
	sb->s_maxbytes = flatfs_max_size(sb->s_blocksize_bits);
	sb->s_time_gran = 1;
	sb->s_export_op = &flatfs_export_ops;
	sb->s_xattr = NULL;
	sb->s_flags |= MS_NOSEC;
	root_i = flatfs_iget(sb, FLATFS_ROOT_INO);
	if (IS_ERR(root_i)) {
		retval = PTR_ERR(root_i);
		goto out;
	}

	sb->s_root = make_flatfs_root(root_i, sb);

	if (sbi->s_mount_opt & FLATFS_MOUNT_FORMAT) {
        brt_recover_all(sb);

		flatfs_init_root_directory(sb, root_i);
	}

	flatfs_recover_truncate_list(sb);
	/* If the FS was not formatted on this mount, scan the meta-data after
	 * truncate list has been processed */
	if ((sbi->s_mount_opt & FLATFS_MOUNT_FORMAT) == 0) {
		flatfs_setup_blocknode_map(sb);
	}

	if (!(sb->s_flags & MS_RDONLY)) {
		u64 mnt_write_time;
		/* update mount time and write time atomically. */
		mnt_write_time = (get_seconds() & 0xFFFFFFFF);
		mnt_write_time = mnt_write_time | (mnt_write_time << 32);

		flatfs_memunlock_range(sb, &super->s_mtime, 8);
		flatfs_memcpy_atomic(&super->s_mtime, &mnt_write_time, 8);
		flatfs_memlock_range(sb, &super->s_mtime, 8);

		flatfs_flush_buffer(&super->s_mtime, 8, false);
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
	}

	printk("flatfs VFS superblock address %016lx\n", sb);
	printk("flatfs in-DRAM superblock address %016lx\n", sbi);
	printk("flatfs in-NVM superblock address %016lx\n", super);
	printk("flatfs root inode(%lu) [%016lx] count[%d] root dentry [%016lx]\n",
           root_i->i_ino, root_i, atomic_read(&root_i->i_count), sb->s_root);

	clear_opt(sbi->s_mount_opt, MOUNTING);
	retval = 0;
	return retval;
out:
	kfree(sbi);
	return retval;
}

int flatfs_statfs(struct dentry *d, struct kstatfs *buf)
{
	struct super_block *sb = d_sb(d);
	unsigned long count = 0;
	struct flatfs_sb_info *sbi = (struct flatfs_sb_info *)sb->s_fs_info;
	int cpu;

	buf->f_type = FLATFS_SUPER_MAGIC;
	buf->f_bsize = sb->s_blocksize;

	count = sbi->block_end;
	buf->f_blocks = sbi->block_end;
	buf->f_bfree = buf->f_bavail = flatfs_count_free_blocks(sb);
	buf->f_files = buf->f_ffree = 0;
	for (cpu = 0; cpu < sbi->num_cpus; cpu ++) {
		buf->f_files += sbi->itable[cpu].s_inodes_used_count;
		buf->f_ffree += (sbi->itable[cpu].s_free_inodes_count);
	}
	buf->f_namelen = FLATFS_NAME_LEN;
	flatfs_dbg_verbose("flatfs_stats: total 4k free blocks 0x%llx\n",
		buf->f_bfree);
	return 0;
}

static int flatfs_show_options(struct seq_file *seq, struct dentry *root)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(root->d_sb);

	seq_printf(seq, ",physaddr=0x%016llx", (u64)sbi->phys_addr);
	if (sbi->initsize)
		seq_printf(seq, ",init=%luk", sbi->initsize >> 10);
	if (sbi->blocksize)
		seq_printf(seq, ",bs=%lu", sbi->blocksize);
	if (sbi->bpi)
		seq_printf(seq, ",bpi=%lu", sbi->bpi);
	if (sbi->num_inodes)
		seq_printf(seq, ",N=%lu", sbi->num_inodes);
	if (sbi->mode != (S_IRWXUGO | S_ISVTX))
		seq_printf(seq, ",mode=%03o", sbi->mode);
	if (uid_valid(sbi->uid))
		seq_printf(seq, ",uid=%u", from_kuid(&init_user_ns, sbi->uid));
	if (gid_valid(sbi->gid))
		seq_printf(seq, ",gid=%u", from_kgid(&init_user_ns, sbi->gid));
	if (test_opt(root->d_sb, ERRORS_RO))
		seq_puts(seq, ",errors=remount-ro");
	if (test_opt(root->d_sb, ERRORS_PANIC))
		seq_puts(seq, ",errors=panic");
	/* memory protection disabled by default */
	if (test_opt(root->d_sb, PROTECT))
		seq_puts(seq, ",wprotect");
	if (test_opt(root->d_sb, HUGEMMAP))
		seq_puts(seq, ",hugemmap");
	if (test_opt(root->d_sb, HUGEIOREMAP))
		seq_puts(seq, ",hugeioremap");
	/* xip not enabled by default */
	if (test_opt(root->d_sb, XIP))
		seq_puts(seq, ",xip");

	return 0;
}

int flatfs_remount(struct super_block *sb, int *mntflags, char *data)
{
	unsigned long old_sb_flags;
	unsigned long old_mount_opt;
	struct flatfs_super_block *ps;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	int ret = -EINVAL;

	/* Store the old options */
	mutex_lock(&sbi->s_lock);
	old_sb_flags = sb->s_flags;
	old_mount_opt = sbi->s_mount_opt;

	if (flatfs_parse_options(data, sbi, 1))
		goto restore_opt;

	sb->s_flags = (sb->s_flags & ~MS_POSIXACL) |
		      ((sbi->s_mount_opt & FLATFS_MOUNT_POSIX_ACL) ? MS_POSIXACL : 0);

	if ((*mntflags & MS_RDONLY) != (sb->s_flags & MS_RDONLY)) {
		u64 mnt_write_time;
		ps = flatfs_get_super(sb);
		/* update mount time and write time atomically. */
		mnt_write_time = (get_seconds() & 0xFFFFFFFF);
		mnt_write_time = mnt_write_time | (mnt_write_time << 32);

		flatfs_memunlock_range(sb, &ps->s_mtime, 8);
		flatfs_memcpy_atomic(&ps->s_mtime, &mnt_write_time, 8);
		flatfs_memlock_range(sb, &ps->s_mtime, 8);

		flatfs_flush_buffer(&ps->s_mtime, 8, false);
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
	}

	mutex_unlock(&sbi->s_lock);
	ret = 0;
	return ret;

restore_opt:
	sb->s_flags = old_sb_flags;
	sbi->s_mount_opt = old_mount_opt;
	mutex_unlock(&sbi->s_lock);
	return ret;
}

static void flatfs_put_super(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	struct flatfs_blocknode *i;
	struct list_head *head = &(sbi->block_inuse_head);

#ifdef CONFIG_FLATFS_DEBUG
	printk("flatfs_put_super: %s\n", sbi->mount_path);
#endif

#ifdef CONFIG_FLATFS_TEST
	if (first_flatfs_super == sbi->virt_addr)
		first_flatfs_super = NULL;
#endif

	/* It's unmount time, so unmap the flatfs memory */
	if (sbi->virt_addr) {
		//flatfs_free_mount_points(sbi);
		flatfs_save_blocknode_mappings(sb);
		flatfs_journal_uninit(sb);
		/* deinit namespace */
		flatfs_namespace_deinit(sbi);
		kfree(sbi->mount_path);
		sbi->virt_addr = NULL;
		flatfs_uninit_blocks(sbi);
	}

	/* Free all the flatfs_blocknodes */
	while (!list_empty(head)) {
		i = list_first_entry(head, struct flatfs_blocknode, link);
		list_del(&i->link);
		flatfs_free_blocknode(sb, i);
	}
	sb->s_fs_info = NULL;
	flatfs_dbgmask = 0;
	kfree(sbi);
}

inline void flatfs_free_transaction(flatfs_transaction_t *trans)
{
    if (!trans->on_stack) {
		kfree(trans->info_start_addr);
        kmem_cache_free(flatfs_transaction_cachep, trans);
    }
}

void __flatfs_free_blocknode(struct flatfs_blocknode *bnode)
{
	kmem_cache_free(flatfs_blocknode_cachep, bnode);
}

void flatfs_free_blocknode(struct super_block *sb, struct flatfs_blocknode *bnode)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	sbi->num_blocknode_allocated--;
	__flatfs_free_blocknode(bnode);
}

inline flatfs_transaction_t *flatfs_alloc_transaction(void)
{
	return (flatfs_transaction_t *)
		kmem_cache_alloc(flatfs_transaction_cachep, GFP_ATOMIC);
}

struct flatfs_blocknode *flatfs_alloc_blocknode(struct super_block *sb)
{
	struct flatfs_blocknode *p;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	p = (struct flatfs_blocknode *)
		kmem_cache_alloc(flatfs_blocknode_cachep, GFP_ATOMIC);
	if (p) {
		sbi->num_blocknode_allocated++;
	}
	return p;
}

static struct inode *flatfs_alloc_inode(struct super_block *sb)
{
	struct flatfs_inode_info *vi;

	vi = kmem_cache_alloc(flatfs_inode_cachep, GFP_ATOMIC);
	if (!vi)
		return NULL;

//	vi->vfs_inode.i_version = 1;
	return &vi->vfs_inode;
}

static void flatfs_i_callback(struct rcu_head *head)
{
	struct inode *inode = container_of(head, struct inode, i_rcu);

	kmem_cache_free(flatfs_inode_cachep, FLATFS_I(inode));
}

static void flatfs_destroy_inode(struct inode *inode)
{
	call_rcu(&inode->i_rcu, flatfs_i_callback);
}

static void init_once(void *foo)
{
	struct flatfs_inode_info *vi = foo;

	vi->i_dir_start_lookup = 0;
	INIT_LIST_HEAD(&vi->i_truncated);
	inode_init_once(&vi->vfs_inode);
}


static int __init init_blocknode_cache(void)
{
	flatfs_blocknode_cachep = kmem_cache_create("flatfs_blocknode_cache",
					sizeof(struct flatfs_blocknode),
					0, (SLAB_RECLAIM_ACCOUNT |
                                        SLAB_MEM_SPREAD), NULL);
	if (flatfs_blocknode_cachep == NULL)
		return -ENOMEM;
	return 0;
}


static int __init init_inodecache(void)
{
	flatfs_inode_cachep = kmem_cache_create("flatfs_inode_cache",
					       sizeof(struct flatfs_inode_info),
					       0, (SLAB_RECLAIM_ACCOUNT |
						   SLAB_MEM_SPREAD), init_once);
	if (flatfs_inode_cachep == NULL)
		return -ENOMEM;
	return 0;
}

static int __init init_transaction_cache(void)
{
	flatfs_transaction_cachep = kmem_cache_create("flatfs_journal_transaction",
			sizeof(flatfs_transaction_t), 0, (SLAB_RECLAIM_ACCOUNT |
			SLAB_MEM_SPREAD), NULL);
	if (flatfs_transaction_cachep == NULL) {
		flatfs_dbg("FLATFS: failed to init transaction cache\n");
		return -ENOMEM;
	}
	return 0;
}

static void destroy_transaction_cache(void)
{
	if (flatfs_transaction_cachep)
		kmem_cache_destroy(flatfs_transaction_cachep);
	flatfs_transaction_cachep = NULL;
}

static void destroy_inodecache(void)
{
	/*
	 * Make sure all delayed rcu free inodes are flushed before
	 * we destroy cache.
	 */
	rcu_barrier();
	kmem_cache_destroy(flatfs_inode_cachep);
}

static void destroy_blocknode_cache(void)
{
	kmem_cache_destroy(flatfs_blocknode_cachep);
}

/*
 * the super block writes are all done "on the fly", so the
 * super block is never in a "dirty" state, so there's no need
 * for write_super.
 */
static struct super_operations flatfs_sops = {
	.alloc_inode	= flatfs_alloc_inode,
	.destroy_inode	= flatfs_destroy_inode,
	.write_inode	= flatfs_write_inode,
	.dirty_inode	= flatfs_dirty_inode,
	.evict_inode	= flatfs_evict_inode,
	.put_super	= flatfs_put_super,
	.statfs		= flatfs_statfs,
	.remount_fs	= flatfs_remount,
	.show_options	= flatfs_show_options,
};

static struct dentry *flatfs_mount(struct file_system_type *fs_type,
				  int flags, const char *dev_name, void *data)
{
	return mount_bdev(fs_type, flags, dev_name, data, flatfs_fill_super);
}

static struct file_system_type flatfs_fs_type = {
	.owner		= THIS_MODULE,
	.name		= "flatfs",
	.mount		= flatfs_mount,
	.kill_sb	= kill_block_super,
};

static struct inode *flatfs_nfs_get_inode(struct super_block *sb,
					 u64 ino, u32 generation)
{
	struct inode *inode;

	if (ino < FLATFS_ROOT_INO)
		return ERR_PTR(-ESTALE);

	inode = flatfs_iget(sb, ino);
	if (IS_ERR(inode))
		return ERR_CAST(inode);

	if (generation && inode->i_generation != generation) {
		/* we didn't find the right inode.. */
		iput(inode);
		return ERR_PTR(-ESTALE);
	}

	return inode;
}

static struct dentry *flatfs_fh_to_dentry(struct super_block *sb,
					 struct fid *fid, int fh_len,
					 int fh_type)
{
	return generic_fh_to_dentry(sb, fid, fh_len, fh_type,
				    flatfs_nfs_get_inode);
}

static struct dentry *flatfs_fh_to_parent(struct super_block *sb,
					 struct fid *fid, int fh_len,
					 int fh_type)
{
	return generic_fh_to_parent(sb, fid, fh_len, fh_type,
				    flatfs_nfs_get_inode);
}

void flatfs_init_ptrees(struct super_block *sb) {
    struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
    memset(flatfs_sb->trees, 0, sizeof(flatfs_sb->trees));
}

brt_tree_t flatfs_get_tree(struct super_block *sb) {
    struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
    size_t n = sizeof(flatfs_sb->trees) / sizeof(*flatfs_sb->trees);
    brt_ptree_t *pt = flatfs_sb->trees;
    brt_tree_t res;
    while (n--) {
        if (pt->state == BRT_PT_NONE) {
            pt->state = BRT_PT_INTACT;
            pt->in_chgpre = 0;
            pt->root = BRT_NOFF;
            brt_pin(&res, pt, FLATFS_SB(sb));
            return res;
        }
        pt++;
    }
    BUG();
}

void flatfs_put_tree(brt_tree_t *tree) {
    tree->ptree->state = BRT_PT_NONE;
    tree->ptree->root = BRT_NOFF;
}

static const struct export_operations flatfs_export_ops = {
	.fh_to_dentry	= flatfs_fh_to_dentry,
	.fh_to_parent	= flatfs_fh_to_parent,
	//.get_parent	= flatfs_get_parent,
};

static int __init init_flatfs_fs(void)
{
	int rc = 0;

	rc = init_blocknode_cache();
	if (rc)
		return rc;

	rc = init_transaction_cache();
	if (rc)
		goto out1;

	rc = init_inodecache();
	if (rc)
		goto out2;

	rc = register_filesystem(&flatfs_fs_type);
	if (rc)
		goto out3;

	return 0;

out3:
	destroy_inodecache();
out2:
	destroy_transaction_cache();
out1:
	destroy_blocknode_cache();
	return rc;
}

static void __exit exit_flatfs_fs(void)
{
	unregister_filesystem(&flatfs_fs_type);
	destroy_inodecache();
	destroy_blocknode_cache();
	destroy_transaction_cache();
}

MODULE_AUTHOR("Miao Cai, Junru Shen <(mcai,gnu_emacs)@hhu.edu.cn>");
MODULE_DESCRIPTION("FlatFS: A Metadata-Optimized NVM File System");
MODULE_LICENSE("GPL");

module_init(init_flatfs_fs)
module_exit(exit_flatfs_fs)
