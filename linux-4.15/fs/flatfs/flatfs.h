/*
 * BRIEF DESCRIPTION
 *
 * Definitions for the FLATFS filesystem.
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
#ifndef __FLATFS_H
#define __FLATFS_H

#include <linux/crc16.h>
#include <linux/mutex.h>
#include <linux/pagemap.h>
#include <linux/rcupdate.h>
#include <linux/types.h>
#include <linux/uio.h>
#include <linux/version.h>
#include <linux/pfn_t.h>
#include <linux/cred.h>
#include <linux/flatfs_define.h>
#include <linux/flatfs.h>
#include <linux/fastr.h>
#include <linux/ppcs.h>

#include "flatfs_def.h"
#include "journal.h"

#define PAGE_SHIFT	  	12
#define PAGE_SHIFT_2M 	21
#define PAGE_SHIFT_1G 	30

#define PAGE_SIZE_4K	0x1000
#define PAGE_SIZE_2M	0x200000
#define PAGE_SIZE_1G	0x40000000

#define FLATFS_VADDR_PRE        0xffff000000000000ul

#define FLATFS_ASSERT(x) \
	do { if (!(x)) {                                                     \
		printk(KERN_WARNING "assertion failed %s:%d: %s\n",     \
	               __FILE__, __LINE__, #x);                         \
	} } while (0)

/*
 * Debug code
 */
#ifdef pr_fmt
#undef pr_fmt
#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt
#endif

/* #define flatfs_dbg(s, args...)         pr_debug(s, ## args) */
#define flatfs_dbg(s, args ...)           pr_info(s, ## args)
#define flatfs_dbg1(s, args ...)
#define flatfs_err(sb, s, args ...)       flatfs_error_mng(sb, s, ## args)
#define flatfs_warn(s, args ...)          pr_warning(s, ## args)
#define flatfs_info(s, args ...)          pr_info(s, ## args)      


extern unsigned int flatfs_dbgmask;
#define FLATFS_DBGMASK_MMAPHUGE          (0x00000001)
#define FLATFS_DBGMASK_MMAP4K            (0x00000002)
#define FLATFS_DBGMASK_MMAPVERBOSE       (0x00000004)
#define FLATFS_DBGMASK_MMAPVVERBOSE      (0x00000008)
#define FLATFS_DBGMASK_VERBOSE           (0x00000010)
#define FLATFS_DBGMASK_TRANSACTION       (0x00000020)

#define flatfs_dbg_mmaphuge(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_MMAPHUGE) ? flatfs_dbg(s, args) : 0)
#define flatfs_dbg_mmap4k(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_MMAP4K) ? flatfs_dbg(s, args) : 0)
#define flatfs_dbg_mmapv(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_MMAPVERBOSE) ? flatfs_dbg(s, args) : 0)
#define flatfs_dbg_mmapvv(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_MMAPVVERBOSE) ? flatfs_dbg(s, args) : 0)

#define flatfs_dbg_verbose(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_VERBOSE) ? flatfs_dbg(s, ##args) : 0)
#define flatfs_dbg_trans(s, args ...)		 \
	((flatfs_dbgmask & FLATFS_DBGMASK_TRANSACTION) ? flatfs_dbg(s, ##args) : 0)

#define flatfs_set_bit                   __test_and_set_bit_le
#define flatfs_clear_bit                 __test_and_clear_bit_le
#define flatfs_find_next_zero_bit        find_next_zero_bit_le

#define clear_opt(o, opt)       (o &= ~FLATFS_MOUNT_ ## opt)
#define set_opt(o, opt)         (o |= FLATFS_MOUNT_ ## opt)
#define test_opt(sb, opt)       (FLATFS_SB(sb)->s_mount_opt & FLATFS_MOUNT_ ## opt)

#define FLATFS_LARGE_INODE_TABLE_SIZE    (0x2000000) // 32MB
/* FLATFS size threshold for using 2M blocks for inode table */
#define FLATFS_LARGE_INODE_TABLE_THREASHOLD    (0x20000000)
#define FLATFS_NUM_INODE_PER_TABLE	(FLATFS_LARGE_INODE_TABLE_SIZE / FLATFS_INODE_SIZE)
#define FLATFS_INODE_USE_THREASHOLD	(260000)

/*
 * flatfs inode flags
 *
 * FLATFS_EOFBLOCKS_FL	There are blocks allocated beyond eof
 */
#define FLATFS_EOFBLOCKS_FL      0x20000000
/* Flags that should be inherited by new inodes from their parent. */
#define FLATFS_FL_INHERITED (FS_SECRM_FL | FS_UNRM_FL | FS_COMPR_FL | \
			    FS_SYNC_FL | FS_NODUMP_FL | FS_NOATIME_FL |	\
			    FS_COMPRBLK_FL | FS_NOCOMP_FL | FS_JOURNAL_DATA_FL | \
			    FS_NOTAIL_FL | FS_DIRSYNC_FL)
/* Flags that are appropriate for regular files (all but dir-specific ones). */
#define FLATFS_REG_FLMASK (~(FS_DIRSYNC_FL | FS_TOPDIR_FL))
/* Flags that are appropriate for non-directories/regular files. */
#define FLATFS_OTHER_FLMASK (FS_NODUMP_FL | FS_NOATIME_FL)
#define FLATFS_FL_USER_VISIBLE (FS_FL_USER_VISIBLE | FLATFS_EOFBLOCKS_FL)

#define INODES_PER_BLOCK(bt) (1 << (blk_type_to_shift[bt] - FLATFS_INODE_BITS))

extern unsigned int blk_type_to_shift[FLATFS_BLOCK_TYPE_MAX];
extern unsigned int blk_type_to_size[FLATFS_BLOCK_TYPE_MAX];

/* ======================= Timing ========================= */
enum timing_category {
	create_t,
	unlink_t,
	readdir_t,
	xip_read_t,
	xip_write_t,
	xip_write_fast_t,
	internal_write_t,
	memcpy_r_t,
	memcpy_w_t,
	alloc_blocks_t,
	new_trans_t,
	add_log_t,
	commit_trans_t,
	mmap_fault_t,
	fsync_t,
	free_tree_t,
	evict_inode_t,
	recovery_t,
	TIMING_NUM,
};

extern const char *Timingstring[TIMING_NUM];
extern unsigned long long Timingstats[TIMING_NUM];
extern u64 Countstats[TIMING_NUM];

extern int measure_timing;
extern int support_clwb;

extern atomic64_t fsync_pages;

typedef struct timespec timing_t;

#define FLATFS_START_TIMING(name, start) \
	{if (measure_timing) getrawmonotonic(&start);}

#define FLATFS_END_TIMING(name, start) \
	{if (measure_timing) { \
		timing_t end; \
		getrawmonotonic(&end); \
		Timingstats[name] += \
			(end.tv_sec - start.tv_sec) * 1000000000 + \
			(end.tv_nsec - start.tv_nsec); \
	} \
	Countstats[name]++; \
	}

/* Function Prototypes */
extern void flatfs_error_mng(struct super_block *sb, const char *fmt, ...);

/* file.c */
extern int flatfs_mmap(struct file *file, struct vm_area_struct *vma);

/* balloc.c */
extern void flatfs_init_blocks(struct flatfs_sb_info *sbi);
extern void flatfs_uninit_blocks(struct flatfs_sb_info *sbi);
int flatfs_setup_blocknode_map(struct super_block *sb);
extern struct flatfs_blocknode *flatfs_alloc_blocknode(struct super_block *sb);
extern void flatfs_free_blocknode(struct super_block *sb, struct flatfs_blocknode *bnode);
extern void flatfs_init_blockmap(struct super_block *sb,
		unsigned long init_used_size);
extern void flatfs_free_block(struct super_block *sb, unsigned long blocknr,
	unsigned short btype);
extern void __flatfs_free_block(struct super_block *sb, unsigned long blocknr,
	unsigned short btype, struct flatfs_blocknode **start_hint);
extern int flatfs_new_block(struct super_block *sb, unsigned long *blocknr,
	unsigned short btype, int zero);
extern unsigned long flatfs_count_free_blocks(struct super_block *sb);

extern inline int flatfs_bread(struct flatfs_sb_info *sbi, unsigned long blocknr);
extern inline int flatfs_bput(struct flatfs_sb_info *sbi, unsigned long blocknr);
extern void flatfs_bget(struct flatfs_sb_info *sbi, unsigned long blocknr, unsigned short btype);

/* dir.c */

/* inode.c */
extern void flatfs_inode_set_cow(struct super_block *sb, struct inode *inode);
extern unsigned int flatfs_free_inode_subtree(struct super_block *sb,
		__le64 root, u32 height, u32 btype, unsigned long last_blocknr);
extern int __flatfs_alloc_blocks(flatfs_transaction_t *trans,
		struct super_block *sb, struct flatfs_inode *pi,
		unsigned long file_blocknr, unsigned int num, bool zero);
extern int flatfs_init_inode_table(struct super_block *sb);
extern int flatfs_alloc_blocks(flatfs_transaction_t *trans, struct inode *inode,
		unsigned long file_blocknr, unsigned int num, bool zero);
extern u64 flatfs_find_data_block(struct inode *inode,
	unsigned long file_blocknr, int write);
int flatfs_set_blocksize_hint(struct super_block *sb, struct flatfs_inode *pi,
		loff_t new_size);
void flatfs_setsize(struct inode *inode, loff_t newsize);
int flatfs_setattr(struct dentry *dentry, struct iattr *attr);

extern struct inode *flatfs_iget(struct super_block *sb, unsigned long ino);
extern void flatfs_put_inode(struct inode *inode);
extern void flatfs_evict_inode(struct inode *inode);
extern struct inode *flatfs_new_inode(flatfs_transaction_t *trans,
	struct inode *dir, umode_t mode);
extern struct inode *flatfs_new_inode_nocaching(flatfs_transaction_t *trans,
	struct inode *dir, umode_t mode);
extern void flatfs_update_isize(struct inode *inode, struct flatfs_inode *pi);
extern void flatfs_update_nlink(struct inode *inode, struct flatfs_inode *pi);
extern void flatfs_update_time(struct inode *inode, struct flatfs_inode *pi);
extern int flatfs_write_inode(struct inode *inode, struct writeback_control *wbc);
extern void flatfs_dirty_inode(struct inode *inode, int flags);
//extern int flatfs_notify_change(struct dentry *dentry, struct iattr *attr);
extern int flatfs_getattr(const struct path *path, struct kstat *stat, u32 request_mask, unsigned int flags);
extern void flatfs_set_inode_flags(struct inode *inode, struct flatfs_inode *pi);
extern void flatfs_get_inode_flags(struct inode *inode, struct flatfs_inode *pi);
extern unsigned long flatfs_find_region(struct inode *inode, loff_t *offset,
		int hole);
extern void flatfs_truncate_del(struct inode *inode);
extern void flatfs_truncate_add(struct inode *inode, u64 truncate_size);

/* ioctl.c */
extern long flatfs_ioctl(struct file *filp, unsigned int cmd, unsigned long arg);
#ifdef CONFIG_COMPAT
extern long flatfs_compat_ioctl(struct file *file, unsigned int cmd,
	unsigned long arg);
#endif

/* super.c */
#ifdef CONFIG_FLATFS_TEST
extern struct flatfs_super_block *get_flatfs_super(void);
#endif
extern void __flatfs_free_blocknode(struct flatfs_blocknode *bnode);
extern struct super_block *flatfs_read_super(struct super_block *sb, void *data,
	int silent);
extern int flatfs_statfs(struct dentry *d, struct kstatfs *buf);
extern int flatfs_remount(struct super_block *sb, int *flags, char *data);
void flatfs_init_ptrees(struct super_block *sb);
brt_tree_t flatfs_get_tree(struct super_block *sb);
void flatfs_put_tree(brt_tree_t *tree);

/* symlink.c */
extern int flatfs_block_symlink(struct inode *inode, const char *symname,
	int len);

/* bplustree.c */

/* namespace.c */
struct walk_desc;
extern long flatfs_namespace_insert(void *tree, brt_key_t key, brt_val_t data, ppcs_t pppcs, ppc_t pppc);
extern int flatfs_namespace_update(void *tree, brt_key_t key, brt_val_t data);
extern int flatfs_namespace_remove(void *tree, brt_key_t key);
extern struct inode *
flatfs_namespace_lookup(void *tree, fastr_t path, ppcs_t *get_ppc, struct walk_desc *wd, int perm_check);

extern void flatfs_namespace_init(struct flatfs_sb_info* sbi, int mount_format);
extern void flatfs_namespace_deinit(struct flatfs_sb_info* sbi);

/* Inline functions start here */

/* Mask out flags that are inappropriate for the given type of inode. */
static inline __le32 flatfs_mask_flags(umode_t mode, __le32 flags)
{
	flags &= cpu_to_le32(FLATFS_FL_INHERITED);
	if (S_ISDIR(mode))
		return flags;
	else if (S_ISREG(mode))
		return flags & cpu_to_le32(FLATFS_REG_FLMASK);
	else
		return flags & cpu_to_le32(FLATFS_OTHER_FLMASK);
}

static inline int flatfs_calc_checksum(u8 *data, int n)
{
	u16 crc = 0;

	crc = crc16(~0, (__u8 *)data + sizeof(__le16), n - sizeof(__le16));
	if (*((__le16 *)data) == cpu_to_le16(crc))
		return 0;
	else
		return 1;
}

struct flatfs_block {
	atomic_t count;
};

struct flatfs_blocknode_lowhigh {
       __le64 block_low;
       __le64 block_high;
};
               
struct flatfs_blocknode {
	struct list_head link;
	unsigned long block_low;
	unsigned long block_high;
};

struct flatfs_inode_info {
	__u32   i_dir_start_lookup;
	struct list_head i_truncated;
	struct inode	vfs_inode;
};

#if 0
static inline struct flatfs_sb_info *FLATFS_SB(struct super_block *sb)
{
	return sb->s_fs_info;
}
#endif

static inline struct flatfs_inode_info *FLATFS_I(struct inode *inode)
{
	return container_of(inode, struct flatfs_inode_info, vfs_inode);
}

/* If this is part of a read-modify-write of the super block,
 * flatfs_memunlock_super() before calling! */
static inline struct flatfs_super_block *flatfs_get_super(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	return (struct flatfs_super_block *)sbi->virt_addr;
}

static inline flatfs_journal_t *flatfs_get_journal(struct super_block *sb)
{
	struct flatfs_super_block *ps = flatfs_get_super(sb);
	return (flatfs_journal_t *)((char *)ps +
			le64_to_cpu(ps->s_journal_offset));
}

#if 0
static inline struct flatfs_inode *flatfs_get_inode_table(struct super_block *sb)
{
	struct flatfs_super_block *ps = flatfs_get_super(sb);

	return (struct flatfs_inode *)((char *)ps +
			le64_to_cpu(ps->s_inode_table_offset));
}
#endif
static inline struct flatfs_inode *flatfs_get_inode_table(struct super_block *sb, int cpu)
{
	struct flatfs_super_block *ps = flatfs_get_super(sb);

	return (struct flatfs_inode *)((char *)ps +
			le64_to_cpu(ps->s_inode_table_offset[cpu]));
}

static inline struct flatfs_super_block *flatfs_get_redund_super(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	return (struct flatfs_super_block *)(sbi->virt_addr + FLATFS_SB_SIZE);
}

/* If this is part of a read-modify-write of the block,
 * flatfs_memunlock_block() before calling! */
static inline void *flatfs_get_block(struct super_block *sb, u64 block)
{
	struct flatfs_super_block *ps = flatfs_get_super(sb);

	return block ? ((void *)ps + block) : NULL;
}

static inline bool nvm_ptr(struct flatfs_sb_info* sbi, void* ptr) {
	return ((unsigned long)ptr >= (unsigned long)sbi->virt_addr &&
		(unsigned long)ptr <= ((unsigned long)sbi->virt_addr + sbi->initsize));
}

static inline void* flatfs_offset_to_address(struct super_block* sb, __le64 offset)
{
	struct flatfs_super_block *ms = flatfs_get_super(sb);
    BUG_ON(!nvm_ptr(FLATFS_SB(sb), (void *) ms + offset));
	return (void*)((unsigned long)ms + offset);
}

static inline __le64 flatfs_address_to_offset(struct super_block* sb, void* address) {
	struct flatfs_super_block *ms = flatfs_get_super(sb);
    BUG_ON(!nvm_ptr(FLATFS_SB(sb), address));
    return ((unsigned long) address - (unsigned long) ms);
}

/* uses CPU instructions to atomically write up to 8 bytes */
static inline void flatfs_memcpy_atomic (void *dst, const void *src, u8 size)
{
	switch (size) {
		case 1: {
			volatile u8 *daddr = dst;
			const u8 *saddr = src;
			*daddr = *saddr;
			break;
		}
		case 2: {
			volatile __le16 *daddr = dst;
			const u16 *saddr = src;
			*daddr = cpu_to_le16(*saddr);
			break;
		}
		case 4: {
			volatile __le32 *daddr = dst;
			const u32 *saddr = src;
			*daddr = cpu_to_le32(*saddr);
			break;
		}
		case 8: {
			volatile __le64 *daddr = dst;
			const u64 *saddr = src;
			*daddr = cpu_to_le64(*saddr);
			break;
		}
		default:
			flatfs_dbg("error: memcpy_atomic called with %d bytes\n", size);
			//BUG();
	}
}

static inline void flatfs_update_time_and_size(struct inode *inode,
	struct flatfs_inode *pi)
{
	__le32 words[2];
	__le64 new_pi_size = cpu_to_le64(i_size_read(inode));

	/* pi->i_size, pi->i_ctime, and pi->i_mtime need to be atomically updated.
 	* So use cmpxchg16b here. */
	words[0] = cpu_to_le32(inode->i_ctime.tv_sec);
	words[1] = cpu_to_le32(inode->i_mtime.tv_sec);
	/* TODO: the following function assumes cmpxchg16b instruction writes
 	* 16 bytes atomically. Confirm if it is really true. */
	cmpxchg_double_local(&pi->i_size, (u64 *)&pi->i_ctime, pi->i_size,
		*(u64 *)&pi->i_ctime, new_pi_size, *(u64 *)words);
}

/* assumes the length to be 4-byte aligned */
static inline void memset_nt(void *dest, uint32_t dword, size_t length)
{
	uint64_t dummy1, dummy2;
	uint64_t qword = ((uint64_t)dword << 32) | dword;

	asm volatile ("movl %%edx,%%ecx\n"
		"andl $63,%%edx\n"
		"shrl $6,%%ecx\n"
		"jz 9f\n"
		"1:      movnti %%rax,(%%rdi)\n"
		"2:      movnti %%rax,1*8(%%rdi)\n"
		"3:      movnti %%rax,2*8(%%rdi)\n"
		"4:      movnti %%rax,3*8(%%rdi)\n"
		"5:      movnti %%rax,4*8(%%rdi)\n"
		"8:      movnti %%rax,5*8(%%rdi)\n"
		"7:      movnti %%rax,6*8(%%rdi)\n"
		"8:      movnti %%rax,7*8(%%rdi)\n"
		"leaq 64(%%rdi),%%rdi\n"
		"decl %%ecx\n"
		"jnz 1b\n"
		"9:     movl %%edx,%%ecx\n"
		"andl $7,%%edx\n"
		"shrl $3,%%ecx\n"
		"jz 11f\n"
		"10:     movnti %%rax,(%%rdi)\n"
		"leaq 8(%%rdi),%%rdi\n"
		"decl %%ecx\n"
		"jnz 10b\n"
		"11:     movl %%edx,%%ecx\n"
		"shrl $2,%%ecx\n"
		"jz 12f\n"
		"movnti %%eax,(%%rdi)\n"
		"12:\n"
		: "=D"(dummy1), "=d" (dummy2) : "D" (dest), "a" (qword), "d" (length) : "memory", "rcx");
}

#define FLATFS_BLOCK_COW	0x1

#define FLATFS_DAX_READ 		0
#define	FLATFS_DAX_WRITE		1

static inline u64 __flatfs_find_data_block(struct super_block *sb, struct flatfs_inode *pi, unsigned long blocknr)
{
	__le64 *level_ptr;
	u64 bp = 0;
	u32 height, bit_shift;
	unsigned int idx;

	height = pi->height;
	bp = le64_to_cpu(pi->root);

	while (height > 0) {
		level_ptr = flatfs_get_block(sb, bp);
		bit_shift = (height - 1) * META_BLK_SHIFT;
		idx = blocknr >> bit_shift;
		bp = le64_to_cpu(level_ptr[idx]);
		if (bp == 0) 
			return 0;
		blocknr = blocknr & ((1 << bit_shift) - 1);
		height--;
	}

	return bp;
}

static inline unsigned int flatfs_inode_blk_shift (struct flatfs_inode *pi)
{
	return blk_type_to_shift[pi->i_blk_type];
}

static inline uint32_t flatfs_inode_blk_size (struct flatfs_inode *pi)
{
	return blk_type_to_size[pi->i_blk_type];
}

static inline struct flatfs_inode *flatfs_get_inode(struct super_block *sb,
						  u64	ino)
{
	struct flatfs_super_block *ps = flatfs_get_super(sb);
	struct flatfs_inode *inode_table;
	u64 bp, block, ino_offset;
	int cpu;

	if (ino == 0)
		return NULL;

	cpu = (ino >> FLATFS_INODE_BITS) / FLATFS_NUM_INODE_PER_TABLE;
	inode_table = flatfs_get_inode_table(sb, cpu);

	block = (((ino >> FLATFS_INODE_BITS) % FLATFS_NUM_INODE_PER_TABLE) << FLATFS_INODE_BITS)
	        >> flatfs_inode_blk_shift(inode_table);
	bp = __flatfs_find_data_block(sb, inode_table, block);

	if (bp == 0) {
		return NULL;
	}

	ino_offset = (ino & (flatfs_inode_blk_size(inode_table) - 1));
	
	return (struct flatfs_inode *)((void *)ps + bp + ino_offset);
}

static inline u64
flatfs_get_addr_off(struct flatfs_sb_info *sbi, void *addr)
{
	FLATFS_ASSERT((addr >= sbi->virt_addr) &&
			(addr < (sbi->virt_addr + sbi->initsize)));
	return (u64)(addr - sbi->virt_addr);
}

static inline u64
flatfs_get_block_off(struct super_block *sb, unsigned long blocknr,
		    unsigned short btype)
{
	return (u64)blocknr << PAGE_SHIFT;
}

static inline unsigned long
flatfs_get_numblocks(unsigned short btype)
{
	unsigned long num_blocks;

	if (btype == FLATFS_BLOCK_TYPE_4K) {
		num_blocks = 1;
	} else if (btype == FLATFS_BLOCK_TYPE_2M) {
		num_blocks = 512;
	} else {
		//btype == FLATFS_BLOCK_TYPE_1G 
		num_blocks = 0x40000;
	}
	return num_blocks;
}

static inline unsigned long
flatfs_get_blocknr(struct super_block *sb, u64 block, unsigned short btype)
{
	return block >> PAGE_SHIFT;
}

static inline unsigned long flatfs_get_pfn(struct super_block *sb, u64 block)
{
	return (FLATFS_SB(sb)->phys_addr + block) >> PAGE_SHIFT;
}

static inline int flatfs_is_mounting(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = (struct flatfs_sb_info *)sb->s_fs_info;
	return sbi->s_mount_opt & FLATFS_MOUNT_MOUNTING;
}

static inline struct flatfs_inode_truncate_item * flatfs_get_truncate_item (
		struct super_block *sb, u64 ino)
{
	struct flatfs_inode *pi = flatfs_get_inode(sb, ino);
	return (struct flatfs_inode_truncate_item *)(pi + 1);
}

static inline struct flatfs_inode_truncate_item * flatfs_get_truncate_list_head (
		struct super_block *sb)
{
	struct flatfs_inode *pi = flatfs_get_inode_table(sb, 0);
	return (struct flatfs_inode_truncate_item *)(pi + 1);
}

static inline void check_eof_blocks(struct super_block *sb, 
		struct flatfs_inode *pi, loff_t size)
{
	if ((pi->i_flags & cpu_to_le32(FLATFS_EOFBLOCKS_FL)) &&
		(size + sb->s_blocksize) > (le64_to_cpu(pi->i_blocks)
			<< sb->s_blocksize_bits))
		pi->i_flags &= cpu_to_le32(~FLATFS_EOFBLOCKS_FL);
}

#include "wprotect.h"

/*
 * Inodes and files operations
 */

/* dir.c */
extern const struct file_operations flatfs_dir_operations;

/* file.c */
extern const struct inode_operations flatfs_file_inode_operations;
extern const struct file_operations flatfs_xip_file_operations;
int flatfs_fsync(struct file *file, loff_t start, loff_t end, int datasync);

/* inode.c */
extern const struct address_space_operations flatfs_aops_xip;

/* bbuild.c */
void flatfs_save_blocknode_mappings(struct super_block *sb);
void flatfs_copy_inode_subtree(struct inode* src, struct inode* dst);


/* namei.c */
extern const struct inode_operations flatfs_dir_inode_operations;
extern const struct inode_flat_operations flatfs_dir_inode_flat_operations;
extern const struct inode_operations flatfs_special_inode_operations;

/* symlink.c */
extern const struct inode_operations flatfs_symlink_inode_operations;

int flatfs_check_integrity(struct super_block *sb,
	struct flatfs_super_block *super);
void *flatfs_ioremap(struct super_block *sb, phys_addr_t phys_addr,
	ssize_t size);

//int flatfs_check_dir_entry(const char *function, struct inode *dir,
//			  struct flatfs_direntry *de, u8 *base,
//			  unsigned long offset);

#if 0
static inline int flatfs_match(int len, const char *const name,
			      struct flatfs_direntry *de)
{
	if (len == de->name_len && de->ino && !memcmp(de->name, name, len))
		return 1;
	return 0;
}

int flatfs_search_dirblock(u8 *blk_base, struct inode *dir, struct qstr *child,
			  unsigned long offset,
			  struct flatfs_direntry **res_dir,
			  struct flatfs_direntry **prev_dir);
#endif

/* flatfs_stats.c */
#define	FLATFS_PRINT_TIMING	0xBCD00010
#define	FLATFS_CLEAR_STATS	0xBCD00011
void flatfs_print_timing_stats(void);
void flatfs_clear_stats(void);

/* permission.c */
extern int flatfs_permission(struct inode* dir, struct inode* inode,
		int permission_type, int acc_mode, int flag, bool isdir);

static inline uid_t flatfs_i_uid_read(const struct inode *inode, struct flatfs_inode* pi)
{
	return from_kuid(inode->i_sb->s_user_ns,
		make_kuid(inode->i_sb->s_user_ns, le16_to_cpu(pi->i_uid)));
}

static inline gid_t flatfs_i_gid_read(const struct inode *inode, struct flatfs_inode* pi)
{
	return from_kgid(inode->i_sb->s_user_ns,
		make_kgid(inode->i_sb->s_user_ns, le16_to_cpu(pi->i_gid)));
}

/*
static inline void flatfs_i_uid_write(struct inode *inode, struct flatfs_inode* pi, uid_t uid)
{
	pi->i_uid = make_kuid(inode->i_sb->s_user_ns, uid);
}

static inline void flatfs_i_gid_write(struct inode *inode, struct flatfs_inode* pi, gid_t gid)
{
	pi->i_gid = make_kgid(inode->i_sb->s_user_ns, gid);
}
*/

static inline bool FLATFS_HAS_UNMAPPED_ID(struct inode *inode, struct flatfs_inode *pi)
{
	return !uid_valid(make_kuid(inode->i_sb->s_user_ns, le16_to_cpu(pi->i_uid))) ||
		!gid_valid(make_kgid(inode->i_sb->s_user_ns, le16_to_cpu(pi->i_gid)));
}

static inline bool flatfs_privileged_wrt_inode_uidgid(struct user_namespace *ns, const struct inode *inode, struct flatfs_inode* pi)
{
	return kuid_has_mapping(ns, make_kuid(NULL, le16_to_cpu(pi->i_uid))) &&
		kgid_has_mapping(ns, make_kgid(NULL, le16_to_cpu(pi->i_gid)));
}

static inline bool flatfs_capable_wrt_inode_uidgid(const struct inode *inode, struct flatfs_inode* pi, int cap)
{
	struct user_namespace *ns = current_user_ns();

	return ns_capable(ns, cap) && flatfs_privileged_wrt_inode_uidgid(ns, inode, pi);
}

int flatfs_register_sysctl(void);

#endif /* __FLATFS_H */
