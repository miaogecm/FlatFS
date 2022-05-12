/*
 * FILE NAME include/linux/flatfs_fs.h
 *
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
#ifndef _LINUX_FLATFS_DEF_H
#define _LINUX_FLATFS_DEF_H

#include <linux/types.h>
#include <linux/magic.h>
#include <linux/flatfs_define.h>

#define	FLATFS_SUPER_MAGIC	0xEFFC

/*
 * The FLATFS filesystem constants/structures
 */

typedef enum {
    BRT_LEAF = 0,
    BRT_INTN = 1
} brt_ndtype_t;

typedef enum {
    BRT_PT_NONE   = 0,
    BRT_PT_INTACT = 1,
    BRT_PT_BROKEN = 2,
    BRT_PT_ISOLAT = 3
} brt_ptstat_t;

typedef struct {
    __u8 state;
    __u8 in_chgpre;
    char padding[6];
    union {
        /* For @BRT_PT_INTACT */
        __le64 root;
        /* For @BRT_PT_BROKEN, points to a page */
        __le64 parts;
    };
} __packed brt_ptree_t;

typedef struct {
    fastr_t src, dst;
    ppcs_t pppcs;
    ppc_t pppc;
} __packed brt_chgpre_opt_t;

typedef struct brt_tree_s brt_tree_t;
typedef struct brt_node_s brt_node_t;
typedef struct brt_ent_s brt_ent_t;
typedef struct brt_it_s brt_it_t;
typedef struct brt_qr_s brt_qr_t;
typedef struct brt_stat_s brt_stat_t;
typedef struct brt_entblk_hdr_s brt_entblk_hdr_t;
typedef struct brt_nodeblk_hdr_s brt_nodeblk_hdr_t;

#define DEFINE_BRT_QR(name, _mode)    \
    brt_qr_t name = { .mode = (_mode) }

/*
 * Maximum number of trees
 */
#define FLATFS_MAX_TREE_N   10

/*
 * Path Walk
 */
#define INO_ANNOTATE_SYMLINK    0x4000000000000000
#define INO_ANNOTATE_MNTPOINT  	0x2000000000000000

/*
 * Mount flags
 */
#define FLATFS_MOUNT_PROTECT 0x000001            /* wprotect CR0.WP */
#define FLATFS_MOUNT_XATTR_USER 0x000002         /* Extended user attributes */
#define FLATFS_MOUNT_POSIX_ACL 0x000004          /* POSIX Access Control Lists */
#define FLATFS_MOUNT_XIP 0x000008                /* Execute in place */
#define FLATFS_MOUNT_ERRORS_CONT 0x000010        /* Continue on errors */
#define FLATFS_MOUNT_ERRORS_RO 0x000020          /* Remount fs ro on errors */
#define FLATFS_MOUNT_ERRORS_PANIC 0x000040       /* Panic on errors */
#define FLATFS_MOUNT_HUGEMMAP 0x000080           /* Huge mappings with mmap */
#define FLATFS_MOUNT_HUGEIOREMAP 0x000100        /* Huge mappings with ioremap */
#define FLATFS_MOUNT_PROTECT_OLD 0x000200        /* wprotect PAGE RW Bit */
#define FLATFS_MOUNT_FORMAT      0x000400        /* was FS formatted on mount? */
#define FLATFS_MOUNT_MOUNTING    0x000800        /* FS currently being mounted */

/*
 * Inode Flags
 */
#define FLATFS_INODE_TYPE_FILE		0x01			/* regular file */
#define	FLATFS_INODE_TYPE_DIR		0x02			/* directory file */
#define FLATFS_INODE_TYPE_SYMLINK	0x04			/* symbolic link file */
#define FLATFS_INODE_TYPE_HDLINK	0x08			/* hard link file */

/*
 * Maximal count of links to a file
 */
#define FLATFS_LINK_MAX          32000

#define FLATFS_DEF_BLOCK_SIZE_4K 4096

#define FLATFS_INODE_SIZE 128    /* must be power of two */
#define FLATFS_INODE_BITS   7

#define FLATFS_NAME_LEN 255
#define FLATFS_PATH_LEN 4096

/* FLATFS supported data blocks */
#define FLATFS_BLOCK_TYPE_4K     0
#define FLATFS_BLOCK_TYPE_2M     1
#define FLATFS_BLOCK_TYPE_1G     2
#define FLATFS_BLOCK_TYPE_MAX    3

#define META_BLK_SHIFT 9

/*
 * Play with this knob to change the default block type.
 * By changing the FLATFS_DEFAULT_BLOCK_TYPE to 2M or 1G,
 * we should get pretty good coverage in testing.
 */
#define FLATFS_DEFAULT_BLOCK_TYPE FLATFS_BLOCK_TYPE_4K

/*
 * Structure of an inode in FLATFS. Things to keep in mind when modifying it.
 * 1) Keep the inode size to within 96 bytes if possible. This is because
 *    a 64 byte log-entry can store 48 bytes of data and we would like
 *    to log an inode using only 2 log-entries
 * 2) root must be immediately after the qw containing height because we update
 *    root and height atomically using cmpxchg16b in flatfs_decrease_btree_height 
 * 3) i_size, i_ctime, and i_mtime must be in that order and i_size must be at
 *    16 byte aligned offset from the start of the inode. We use cmpxchg16b to
 *    update these three fields atomically.
 */
struct flatfs_inode {
	/* first 48 bytes */
	__le16	i_rsvd;         /* reserved. used to be checksum */
	u8	    height;         /* height of data b-tree; max 3 for now */
	u8	    i_blk_type;     /* data block size this inode uses */
	__le32	i_flags;            /* Inode flags */
	__le64	root;               /* btree root. must be below qw w/ height */
	__le64	i_size;             /* Size of data in bytes,   */
	__le32	i_ctime;            /* Inode modification time */
	__le32	i_mtime;            /* Inode b-tree Modification time */
	__le32	i_dtime;            /* Deletion Time */
	__le16	i_mode;             /* File mode */
	__le16	i_links_count;      /* Links count */
	__le64	i_blocks;           /* Blocks count */

	/* second 48 bytes */
	__le64	i_xattr;            /* Extended attribute block */
	__le32	i_uid;              /* Owner Uid */
	__le32	i_gid;              /* Group Id */
	__le32	i_generation;       /* File version (for NFS) */
	__le32	i_atime;            /* Access time */

	struct {
		__le32 rdev;    /* major/minor # */
	} dev;              /* device inode */
	u8		i_types;	/* File Types */
	char	padding[3]; /* pad to ensure truncate_item starts 8-byte aligned */
} __attribute__((packed));

#define SHADOW_DENTRY_INO		0x1FFFFFFFFFFFFFFF

/* This is a per-inode structure and follows immediately after the 
 * struct flatfs_inode. It is used to implement the truncate linked list and is 
 * by flatfs_truncate_add(), flatfs_truncate_del(), and flatfs_recover_truncate_list()
 * functions to manage the truncate list */
struct flatfs_inode_truncate_item {
	__le64	i_truncatesize;     /* Size of truncated inode */
	__le64  i_next_truncate;    /* inode num of the next truncated inode */
} __attribute__((packed));

/*
 * #define FLATFS_NAME_LEN (FLATFS_INODE_SIZE - offsetof(struct flatfs_inode,
 *         i_d.d_name) - 1)
 */

/* #define FLATFS_SB_SIZE 128 */ /* must be power of two */
/* #define FLATFS_SB_SIZE 512 */ /* must be power of two */
#define FLATFS_SB_SIZE 12288

struct flatfs_journal {
	__le64     base;
	__le32     size;
	__le32     head;
	/* the next three fields must be in the same order and together.
	 * tail and gen_id must fall in the same 8-byte quadword */
	__le32     tail;
	__le16     gen_id;   /* generation id of the log */
	__le16     pad;
	__le16     redo_logging;
} __attribute__((packed));

typedef struct flatfs_journal flatfs_journal_t;

/*
 * s_node_blocknr and s_entry_blocknr in FlatFS super block is the block number
 * of address block of node blocks and entry blocks. The address block stores 
 * a number of flatfs_block_item which describe the detailed block information
 * of node block and entry block.
 */
struct flatfs_block_item {
	__le32 i_type; /* block type: 1 G 2M 4K */
	__le64 i_blocknr; /* block number */
} __attribute__((packed));

/*
 * Structure of the super block in FLATFS
 * The fields are partitioned into static and dynamic fields. The static fields
 * never change after file system creation. This was primarily done because
 * flatfs_get_block() returns NULL if the block offset is 0 (helps in catching
 * bugs). So if we modify any field using journaling (for consistency), we 
 * will have to modify s_sum which is at offset 0. So journaling code fails.
 * This (static+dynamic fields) is a temporary solution and can be avoided
 * once the file system becomes stable and flatfs_get_block() returns correct
 * pointers even for offset 0.
 */
struct flatfs_super_block {
	/* static fields. they never change after file system creation.
	 * checksum only validates up to s_start_dynamic field below */
	__le16		s_sum;              /* checksum of this sb */
	__le16		s_magic;            /* magic signature */
	__le32		s_blocksize;        /* blocksize in bytes */
	__le64		s_size;             /* total size of fs in bytes */
	char		s_volume_name[16];  /* volume name */
	/* points to the location of flatfs_journal_t */
	__le64          s_journal_offset;
	/* points to the location of struct flatfs_inode for the inode table */
	__le64          s_inode_table_offset[FLATFS_NCPU];

	__le64       s_start_dynamic; 

	/* all the dynamic fields should go here */
	/* s_mtime and s_wtime should be together and their order should not be
	 * changed. we use an 8 byte write to update both of them atomically */
	__le32		s_mtime;            /* mount time */
	__le32		s_wtime;            /* write time */
	/* fields for fast mount support. Always keep them together */
	__le64		s_num_blocknode_allocated;
	__le64		s_num_free_blocks;

	/* after flatfs is mounted, these fields are cached in flatfs_sb_info, only
	 * updated during umount. There is no false sharing problem during running */
	__le32		s_inodes_count[FLATFS_NCPU];
	__le32		s_free_inodes_count[FLATFS_NCPU];
	__le32		s_inodes_used_count[FLATFS_NCPU];
	__le32		s_free_inode_hint[FLATFS_NCPU];

	__le64  	s_node_blocknr; /* start node block number */
	__le64 		s_entry_blocknr; /* start entry block number */

    brt_ptree_t trees[FLATFS_MAX_TREE_N];
} __attribute__((packed));

#define FLATFS_SB_STATIC_SIZE(ps) ((u64)&ps->s_start_dynamic - (u64)ps)

/* the above fast mount fields take total 32 bytes in the super block */
#define FLATFS_FAST_MOUNT_FIELD_SIZE  (36)

/* The root inode follows immediately after the redundant super block */
#define FLATFS_ROOT_INO (FLATFS_INODE_SIZE)
#define FLATFS_BLOCKNODE_IN0 (FLATFS_ROOT_INO + FLATFS_INODE_SIZE)

/* INODE HINT  START at 3 */ 
#define FLATFS_FREE_INODE_HINT_START      (3)

/* ======================= Write ordering ========================= */

#define CACHELINE_SIZE  (64)
#define CACHELINE_MASK  (~(CACHELINE_SIZE - 1))
#define CACHELINE_ALIGN(addr) (((addr)+CACHELINE_SIZE-1) & CACHELINE_MASK)

#define X86_FEATURE_PCOMMIT	( 9*32+22) /* PCOMMIT instruction */
#define X86_FEATURE_CLFLUSHOPT	( 9*32+23) /* CLFLUSHOPT instruction */
#define X86_FEATURE_CLWB	( 9*32+24) /* CLWB instruction */

static inline bool arch_has_pcommit(void)
{
	return static_cpu_has(X86_FEATURE_PCOMMIT);
}

static inline bool arch_has_clwb(void)
{
	return static_cpu_has(X86_FEATURE_CLWB);
}

extern int support_clwb;
extern int support_pcommit;

#define _mm_clflush(addr)\
	asm volatile("clflush %0" : "+m" (*(volatile char *)(addr)))
#define _mm_clflushopt(addr)\
	asm volatile(".byte 0x66; clflush %0" : "+m" (*(volatile char *)(addr)))
#define _mm_clwb(addr)\
	asm volatile(".byte 0x66; xsaveopt %0" : "+m" (*(volatile char *)(addr)))
#define _mm_pcommit()\
	asm volatile(".byte 0x66, 0x0f, 0xae, 0xf8")

/* Provides ordering from all previous clflush too */
static inline void PERSISTENT_MARK(void)
{
	/* TODO: Fix me. */
}

static inline void PERSISTENT_BARRIER(void)
{
	asm volatile ("sfence\n" : : );
	if (support_pcommit) {
		/* Do nothing */
	}
}

static inline void flatfs_flush_buffer(void *buf, uint32_t len, bool fence)
{
	uint32_t i;
	len = len + ((unsigned long)(buf) & (CACHELINE_SIZE - 1));
	if (support_clwb) {
		for (i = 0; i < len; i += CACHELINE_SIZE)
			_mm_clwb(buf + i);
	} else {
		for (i = 0; i < len; i += CACHELINE_SIZE)
			_mm_clflush(buf + i);
	}
	/* Do a fence only if asked. We often don't need to do a fence
	 * immediately after clflush because even if we get context switched
	 * between clflush and subsequent fence, the context switch operation
	 * provides implicit fence. */
	if (fence)
		PERSISTENT_BARRIER();
}

static inline void flatfs_mark_region(void *start, unsigned long *victims, void *buf, uint32_t len) {
    uint32_t i;
    BUG_ON((unsigned long) start % CACHELINE_SIZE != 0);
    len += ((unsigned long)(buf) & (CACHELINE_SIZE - 1));
    for (i = 0; i < len; i += CACHELINE_SIZE) {
        __set_bit((buf - start + i) / CACHELINE_SIZE, victims);
    }
}

static inline void flatfs_flush_regions(void *start, unsigned long *victims, bool fence) {
    uint32_t i;
    BUG_ON((unsigned long) start % CACHELINE_SIZE != 0);
	if (support_clwb) {
        for_each_set_bit(i, victims, BITS_PER_LONG) {
            _mm_clwb(start + i * CACHELINE_SIZE);
        }
    } else {
        for_each_set_bit(i, victims, BITS_PER_LONG) {
            _mm_clflush(start + i * CACHELINE_SIZE);
        }
    }
	if (fence) {
        PERSISTENT_BARRIER();
    }
}

#endif /* _LINUX_FLATFS_DEF_H */
