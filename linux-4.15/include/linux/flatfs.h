/*
 * FlatFS: A Metadata-Optimized NVM File System
 * 
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */

#ifndef FLATFS_H
#define FLATFS_H

#include <linux/types.h>
#include <linux/list.h>
#include <linux/mutex.h>
#include <linux/atomic.h>
#include <linux/wait.h>
#include <linux/uidgid.h>
#include <linux/fs.h>
#include <linux/flatfs_define.h>
#include <linux/flatfs_debug.h>
#include <linux/fastr.h>
#include "ppcs.h"

#define CTLCHR_COMPONENT_SEPARATOR      '\x01'
#define CTLCHR_PREFIX_INTERCEPTOR       '\x7f'

#define cacheline_size  		64

typedef fastr_t brt_key_t;
typedef ino_t   brt_val_t;

struct block_device;
struct super_block;
struct task_struct;
struct brt_tree_s;

/*
 * FLATFS super-block data in memory
 */
struct flatfs_sb_info {
	/*
	 * base physical and virtual address of FLATFS (which is also
	 * the pointer to the super block)
	 */
	struct block_device *s_bdev;
	phys_addr_t	phys_addr;
	void		*virt_addr;
	struct list_head block_inuse_head;
	unsigned long	block_start; /* first block number */
	unsigned long	block_end; /* last block number */
	unsigned long	num_free_blocks;
	struct mutex 	s_lock;	/* protects the SB's buffer-head */

	/*
	 * Backing store option:
	 * 1 = no load, 2 = no store,
	 * else do both
	 */
	unsigned int	flatfs_backing_option;

	/* Mount options */
	unsigned long	bpi;
	unsigned long	num_inodes;
	unsigned long	blocksize;
	unsigned long	initsize;
	unsigned long	s_mount_opt;
	kuid_t		uid;    /* Mount uid for root directory */
	kgid_t		gid;    /* Mount gid for root directory */
	umode_t		mode;   /* Mount mode for root directory */
	atomic_t	next_generation;

	unsigned long num_blocknode_allocated;

    unsigned int num_cpus;

    struct domain     *root_domain;

	struct super_block* super; /* super_block in VFS */

	/* inode tracking, it can avoid false sharing */
	struct {
		struct mutex 	inode_table_mutex;
		unsigned int	s_inodes_count; /* total inodes count (used or free) */
		unsigned int	s_free_inodes_count; /* free inodes count */
		unsigned int	s_inodes_used_count;
		unsigned int	s_free_inode_hint;
	}__attribute__((__aligned__(cacheline_size))) itable[FLATFS_NCPU];

	/* track all 4K blocks */
	struct flatfs_block* nvm_blocks;
	unsigned int order;

	/* Journaling related structures */
	uint32_t    next_transaction_id;
	uint32_t    jsize;
	void       *journal_base_addr;
	spinlock_t  journal_lock;
	struct task_struct *log_cleaner_thread;
	wait_queue_head_t  log_cleaner_wait;
	bool redo_log;

	/* truncate list related structures */
	struct list_head s_truncate;
	struct mutex s_truncate_lock;

	/* mount related structures */
	//const char* mount_path; /* path of mount point */
	int			mount_path_len; /* length of mount path, excluding \0 */
	char*		mount_path;

    struct list_head leaf_list_head;
};

enum {
    WALK_NORMAL = 0,
	WALK_TRAILING_MNTPOINT,
    WALK_TRAILING_SYMLINK,
	WALK_MEET_MNTPOINT,
	WALK_MEET_SYMLINK,
};

struct walk_desc {
    size_t skip;
	int type;
};

struct node_blk_list {
	spinlock_t free_leaf_node_list_locks;
	struct list_head free_leaf_node_lists;
	spinlock_t free_internal_node_list_locks;
	struct list_head free_internal_node_lists;
} __attribute__((__aligned__(cacheline_size)));

struct entry_blk_list {
	spinlock_t free_entry_block_list_locks;
	struct list_head free_entry_block_lists;
	spinlock_t inuse_entry_block_list_locks;
	struct list_head inuse_entry_block_lists;
} __attribute__((__aligned__(cacheline_size)));

static inline struct flatfs_sb_info *FLATFS_SB(struct super_block *sb)
{
	return sb->s_fs_info;
}

/* bplustree.c */

/* namespace.c */
struct walk_desc;
extern struct inode *
flatfs_namespace_lookup(void *tree, fastr_t path, ppcs_t *get_ppc, struct walk_desc *wd, int perm_check);
extern void flatfs_namespace_stat(struct flatfs_sb_info *sbi);

/* inode.c */
extern struct inode *flatfs_iget(struct super_block *sb, unsigned long ino);
extern bool flatfs_inode_is_reg(struct inode* inode);
extern bool flatfs_inode_is_symlink(struct inode* inode);
extern bool flatfs_inode_is_dir(struct inode* inode);

#endif
