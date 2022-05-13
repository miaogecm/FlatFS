/*
 * PMFS emulated persistence. This file contains code to 
 * handle data blocks of various sizes efficiently.
 *
 * Persistent Memory File System
 * Copyright (c) 2012-2013, Intel Corporation.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
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
#include <linux/bitops.h>
#include "flatfs.h"

#if 0
static unsigned int log2(unsigned int v)
{
	unsigned int r;
	unsigned int shift;

	r = (v > 0xFFFF) << 4; v >>= r;
	shift = (v > 0xFF) << 3; v >>= shift; r |= shift;
	shift = (v > 0xF) << 2; v >>= shift; r |= shift;
	shift = (v > 0x3) << 1; v >>= shift; r |= shift;
	r |= (v >> 1);

	return r;
}

void flatfs_init_blocks(struct flatfs_sb_info *sbi)
{
	unsigned int order = log2(sbi->initsize / PAGE_SIZE * sizeof(struct flatfs_block) / PAGE_SIZE);
	char *start;
	
	start = (char*)__get_free_pages(GFP_KERNEL, order);

	/* zeroed out all pages */
	memset(start, 0, (1 << order) * PAGE_SIZE);

	sbi->nvm_blocks = (struct flatfs_block*)start;
	sbi->order = order;
}

void flatfs_uninit_blocks(struct flatfs_sb_info *sbi)
{
	free_pages((unsigned long)sbi->nvm_blocks, sbi->order);
}

void flatfs_bget(struct flatfs_sb_info *sbi, unsigned long blocknr, 
		unsigned short btype) {
	struct flatfs_block* blocks = sbi->nvm_blocks;
	int num_pages;
	int i, num;

	num_pages = blk_type_to_size[btype] >> PAGE_SHIFT;

	for (i = 0, num = blocknr; i < num_pages; i ++, num ++)
		atomic_inc(&blocks[num].count);
}

inline int flatfs_bput(struct flatfs_sb_info *sbi, unsigned long blocknr) {
	return atomic_dec_return(&sbi->nvm_blocks[blocknr].count);
}

inline int flatfs_bread(struct flatfs_sb_info *sbi, unsigned long blocknr) {
	return atomic_read(&sbi->nvm_blocks[blocknr].count);
}
#endif 

void flatfs_init_blocks(struct flatfs_sb_info *sbi)
{
}

void flatfs_uninit_blocks(struct flatfs_sb_info *sbi)
{
}

void flatfs_bget(struct flatfs_sb_info *sbi, unsigned long blocknr, 
		unsigned short btype) {
}

inline int flatfs_bput(struct flatfs_sb_info *sbi, unsigned long blocknr) {
	return 0;
}

inline int flatfs_bread(struct flatfs_sb_info *sbi, unsigned long blocknr) {
	return 0;
}


void flatfs_init_blockmap(struct super_block *sb, unsigned long init_used_size)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	unsigned long num_used_block;
	struct flatfs_blocknode *blknode;

	num_used_block = (init_used_size + sb->s_blocksize - 1) >>
		sb->s_blocksize_bits;

	blknode = flatfs_alloc_blocknode(sb);
	if (blknode == NULL)
		FLATFS_ASSERT(0);
	blknode->block_low = sbi->block_start;
	blknode->block_high = sbi->block_start + num_used_block - 1;
	sbi->num_free_blocks -= num_used_block;
	list_add(&blknode->link, &sbi->block_inuse_head);
}

static struct flatfs_blocknode *flatfs_next_blocknode(struct flatfs_blocknode *i,
						  struct list_head *head)
{
	if (list_is_last(&i->link, head))
		return NULL;
	return list_first_entry(&i->link, typeof(*i), link);
}

/* Caller must hold the super_block lock.  If start_hint is provided, it is
 * only valid until the caller releases the super_block lock. */
void __flatfs_free_block(struct super_block *sb, unsigned long blocknr,
		      unsigned short btype, struct flatfs_blocknode **start_hint)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	struct list_head *head = &(sbi->block_inuse_head);
	unsigned long new_block_low;
	unsigned long new_block_high;
	unsigned long num_blocks = 0;
	struct flatfs_blocknode *i;
	struct flatfs_blocknode *free_blocknode= NULL;
	struct flatfs_blocknode *curr_node;

	num_blocks = flatfs_get_numblocks(btype);
	new_block_low = blocknr;
	new_block_high = blocknr + num_blocks - 1;

	BUG_ON(list_empty(head));

	if (start_hint && *start_hint &&
	    new_block_low >= (*start_hint)->block_low)
		i = *start_hint;
	else
		i = list_first_entry(head, typeof(*i), link);

	list_for_each_entry_from(i, head, link) {

		if (new_block_low > i->block_high) {
			/* skip to next blocknode */
			continue;
		}

		if ((new_block_low == i->block_low) &&
			(new_block_high == i->block_high)) {
					
			/* fits entire datablock */
			if (flatfs_bput(sbi, new_block_low))
				return;
#ifdef CONFIG_FLATFS_DEBUG
			//printk("free block [%lu %lu] count %d btype %d\n", 
			//		new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
#endif
			if (start_hint)
				*start_hint = flatfs_next_blocknode(i, head);
			list_del(&i->link);
			free_blocknode = i;
			sbi->num_blocknode_allocated--;
			sbi->num_free_blocks += num_blocks;
			goto block_found;
		}
		if ((new_block_low == i->block_low) &&
			(new_block_high < i->block_high)) {
			/* Aligns left */
			if (flatfs_bput(sbi, new_block_low))
				return;
#ifdef CONFIG_FLATFS_DEBUG
			//printk("free block [%lu %lu] count %d btype %d\n", 
			//		new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
#endif
			i->block_low = new_block_high + 1;
			sbi->num_free_blocks += num_blocks;
			if (start_hint)
				*start_hint = i;
			goto block_found;
		}
		if ((new_block_low > i->block_low) && 
			(new_block_high == i->block_high)) {
			/* Aligns right */
			if (flatfs_bput(sbi, new_block_low))
				return;
#ifdef CONFIG_FLATFS_DEBUG
			//printk("free block [%lu %lu] count %d btype %d\n", 
			//		new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
#endif
			i->block_high = new_block_low - 1;
			sbi->num_free_blocks += num_blocks;
			if (start_hint)
				*start_hint = flatfs_next_blocknode(i, head);
			goto block_found;
		}
		if ((new_block_low > i->block_low) &&
			(new_block_high < i->block_high)) {
			/* Aligns somewhere in the middle */
			if (flatfs_bput(sbi, new_block_low))
				return;

#ifdef CONFIG_FLATFS_DEBUG
			//printk("free block [%lu %lu] count %d btype %d\n", 
			//		new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
#endif
			curr_node = flatfs_alloc_blocknode(sb);
			FLATFS_ASSERT(curr_node);
			if (curr_node == NULL) {
				/* returning without freeing the block*/
				goto block_found;
			}
			curr_node->block_low = new_block_high + 1;
			curr_node->block_high = i->block_high;
			i->block_high = new_block_low - 1;
			list_add(&curr_node->link, &i->link);
			sbi->num_free_blocks += num_blocks;
			if (start_hint)
				*start_hint = curr_node;
			goto block_found;
		}
	}

	flatfs_error_mng(sb, "Unable to free block [%lu %lu] count %d btype %d\n", 
		new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
	BUG();

block_found:

	if (free_blocknode)
		__flatfs_free_blocknode(free_blocknode);
}

void flatfs_free_block(struct super_block *sb, unsigned long blocknr,
		      unsigned short btype)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	mutex_lock(&sbi->s_lock);
	__flatfs_free_block(sb, blocknr, btype, NULL);
	mutex_unlock(&sbi->s_lock);
}

int flatfs_new_block(struct super_block *sb, unsigned long *blocknr,
	unsigned short btype, int zero)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	struct list_head *head = &(sbi->block_inuse_head);
	struct flatfs_blocknode *i, *next_i;
	struct flatfs_blocknode *free_blocknode= NULL;
	void *bp;
	unsigned long num_blocks = 0;
	struct flatfs_blocknode *curr_node;
	int errval = 0;
	bool found = 0;
	unsigned long next_block_low;
	unsigned long new_block_low;
	unsigned long new_block_high;

	num_blocks = flatfs_get_numblocks(btype);

	mutex_lock(&sbi->s_lock);

	list_for_each_entry(i, head, link) {
		if (i->link.next == head) {
			next_i = NULL;
			next_block_low = sbi->block_end;
		} else {
			next_i = list_entry(i->link.next, typeof(*i), link);
			next_block_low = next_i->block_low;
		}

		new_block_low = (i->block_high + num_blocks) & ~(num_blocks - 1);
		new_block_high = new_block_low + num_blocks - 1;

		if (new_block_high >= next_block_low) {
			/* Does not fit - skip to next blocknode */
			continue;
		}

		if ((new_block_low == (i->block_high + 1)) &&
			(new_block_high == (next_block_low - 1)))
		{
			/* Fill the gap completely */
			if (next_i) {
				i->block_high = next_i->block_high;
				list_del(&next_i->link);
				free_blocknode = next_i;
				sbi->num_blocknode_allocated--;
			} else {
				i->block_high = new_block_high;
			}
			found = 1;
			break;
		}

		if ((new_block_low == (i->block_high + 1)) &&
			(new_block_high < (next_block_low - 1))) {
			/* Aligns to left */
			i->block_high = new_block_high;
			found = 1;
			break;
		}

		if ((new_block_low > (i->block_high + 1)) &&
			(new_block_high == (next_block_low - 1))) {
			/* Aligns to right */
			if (next_i) {
				/* right node exist */
				next_i->block_low = new_block_low;
			} else {
				/* right node does NOT exist */
				curr_node = flatfs_alloc_blocknode(sb);
				FLATFS_ASSERT(curr_node);
				if (curr_node == NULL) {
					errval = -ENOSPC;
					break;
				}
				curr_node->block_low = new_block_low;
				curr_node->block_high = new_block_high;
				list_add(&curr_node->link, &i->link);
			}
			found = 1;
			break;
		}

		if ((new_block_low > (i->block_high + 1)) &&
			(new_block_high < (next_block_low - 1))) {
			/* Aligns somewhere in the middle */
			curr_node = flatfs_alloc_blocknode(sb);
			FLATFS_ASSERT(curr_node);
			if (curr_node == NULL) {
				errval = -ENOSPC;
				break;
			}
			curr_node->block_low = new_block_low;
			curr_node->block_high = new_block_high;
			list_add(&curr_node->link, &i->link);
			found = 1;
			break;
		}
	}
	
	if (found == 1) {
		sbi->num_free_blocks -= num_blocks;
	}	

	mutex_unlock(&sbi->s_lock);

	if (free_blocknode)
		__flatfs_free_blocknode(free_blocknode);

	if (found == 0) {
		return -ENOSPC;
	}

	if (zero) {
		size_t size;
		bp = flatfs_get_block(sb, flatfs_get_block_off(sb, new_block_low, btype));
		flatfs_memunlock_block(sb, bp); //TBDTBD: Need to fix this
		if (btype == FLATFS_BLOCK_TYPE_4K)
			size = 0x1 << 12;
		else if (btype == FLATFS_BLOCK_TYPE_2M)
			size = 0x1 << 21;
		else
			size = 0x1 << 30;
		memset_nt(bp, 0, size);
		flatfs_memlock_block(sb, bp);
	}

	flatfs_bget(sbi, new_block_low, btype);

#ifdef CONFIG_FLATFS_DEBUG
	//printk("alloc block [%lu %lu] count %d btype %d\n", new_block_low, new_block_high, flatfs_bread(sbi, new_block_low), btype);
#endif
	
	*blocknr = new_block_low;
	
	return errval;
}

unsigned long flatfs_count_free_blocks(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	return sbi->num_free_blocks;
}
