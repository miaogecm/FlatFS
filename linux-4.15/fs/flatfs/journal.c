/*
 * PMFS journaling facility. This file contains code to log changes to flatfs
 * meta-data to facilitate consistent meta-data updates against arbitrary
 * power and system failures.
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
 
#include <linux/module.h>
#include <linux/string.h>
#include <linux/init.h>
#include <linux/vfs.h>
#include <linux/uaccess.h>
#include <linux/mm.h>
#include <linux/mutex.h>
#include <linux/sched.h>
#include <linux/kthread.h>
#include "flatfs.h"
#include "journal.h"

static void dump_transaction(struct flatfs_sb_info *sbi,
		flatfs_transaction_t *trans)
{
	int i;
	flatfs_logentry_t *le = trans->start_addr;

	for (i = 0; i < trans->num_entries; i++) {
		flatfs_dbg_trans("ao %llx tid %x gid %x type %x sz %x\n",
			le->addr_offset, le->transaction_id, le->gen_id,
			le->type, le->size);
		le++;
	}
}

static inline uint32_t next_log_entry(uint32_t jsize, uint32_t le_off)
{
	le_off = le_off + LOGENTRY_SIZE;
	if (le_off >= jsize)
		le_off = 0;
	return le_off;
}

static inline uint32_t prev_log_entry(uint32_t jsize, uint32_t le_off)
{
	if (le_off == 0)
		le_off = jsize;
	le_off = le_off - LOGENTRY_SIZE;
	return le_off;
}

static inline uint16_t next_gen_id(uint16_t gen_id)
{
	gen_id++;
	/* check for wraparound */
	if (gen_id == 0)
		gen_id++;
	return gen_id;
}

static inline uint16_t prev_gen_id(uint16_t gen_id)
{
	gen_id--;
	/* check for wraparound */
	if (gen_id == 0)
		gen_id--;
	return gen_id;
}

/* Undo a valid log entry */
static inline void flatfs_undo_logentry(struct super_block *sb,
	flatfs_logentry_t *le)
{
	char *data;

	if (le->size > 0) {
		data = flatfs_get_block(sb, le64_to_cpu(le->addr_offset));
		/* Undo changes by flushing the log entry to flatfs */
		flatfs_memunlock_range(sb, data, le->size);
		memcpy(data, le->data, le->size);
		flatfs_memlock_range(sb, data, le->size);
		flatfs_flush_buffer(data, le->size, false);
	}
}

/* can be called during journal recovery or transaction abort */
/* We need to Undo in the reverse order */
static void flatfs_undo_transaction(struct super_block *sb,
		flatfs_transaction_t *trans)
{
	flatfs_logentry_t *le;
	int i;
	uint16_t gen_id = trans->gen_id;

	le = trans->start_addr + trans->num_used;
	le--;
	for (i = trans->num_used - 1; i >= 0; i--, le--) {
		if (gen_id == le16_to_cpu(le->gen_id))
			flatfs_undo_logentry(sb, le);
	}
}

/* can be called by either during log cleaning or during journal recovery */
static void flatfs_flush_transaction(struct super_block *sb,
		flatfs_transaction_t *trans)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
    flatfs_le_info_t *lei = trans->info_start_addr;
	int i;
	char *data;

    if (trans->on_stack) {
        return;
    }

	for (i = 0; i < trans->num_used; i++, lei++) {
		if (lei->size) {
			data = lei->ptr;
			if (unlikely(sbi->redo_log)) {
				BUG();
			} else {
                flatfs_flush_buffer(data, lei->size, false);
            }
		}
	}
}

static inline void invalidate_gen_id(flatfs_logentry_t *le)
{
	le->gen_id = 0;
	flatfs_flush_buffer(le, LOGENTRY_SIZE, false);
}

/* can be called by either during log cleaning or during journal recovery */
static void flatfs_invalidate_logentries(struct super_block *sb,
		flatfs_transaction_t *trans)
{
	flatfs_logentry_t *le = trans->start_addr;
	int i;

	flatfs_memunlock_range(sb, trans->start_addr,
			trans->num_entries * LOGENTRY_SIZE);
	for (i = 0; i < trans->num_entries; i++) {
		invalidate_gen_id(le);
		if (le->type == LE_START) {
			PERSISTENT_MARK();
			PERSISTENT_BARRIER();
		}
		le++;
	}
	flatfs_memlock_range(sb, trans->start_addr,
			trans->num_entries * LOGENTRY_SIZE);
}

/* can be called by either during log cleaning or during journal recovery */
static void flatfs_redo_transaction(struct super_block *sb,
		flatfs_transaction_t *trans, bool recover)
{
	flatfs_logentry_t *le = trans->start_addr;
	int i;
	uint16_t gen_id = trans->gen_id;
	char *data;

	for (i = 0; i < trans->num_entries; i++) {
		if (gen_id == le16_to_cpu(le->gen_id) && le->size > 0) {
			data = flatfs_get_block(sb,le64_to_cpu(le->addr_offset));
			/* flush data if we are called during recovery */
			if (recover) {
				flatfs_memunlock_range(sb, data, le->size);
				memcpy(data, le->data, le->size);
				flatfs_memlock_range(sb, data, le->size);
			}
			flatfs_flush_buffer(data, le->size, false);
		}
		le++;
	}
}

/* recover the transaction ending at a valid log entry *le */
/* called for Undo log and traverses the journal backward */
static uint32_t flatfs_recover_transaction(struct super_block *sb, uint32_t head,
		uint32_t tail, flatfs_logentry_t *le)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_transaction_t trans;
	bool cmt_or_abrt_found = false, start_found = false;
	uint16_t gen_id = le16_to_cpu(le->gen_id);

	memset(&trans, 0, sizeof(trans));
	trans.transaction_id = le32_to_cpu(le->transaction_id);
	trans.gen_id = gen_id;

	do {
		trans.num_entries++;
		trans.num_used++;

		if (gen_id == le16_to_cpu(le->gen_id)) {
			/* Handle committed/aborted transactions */
			if (le->type & LE_COMMIT || le->type & LE_ABORT)
				cmt_or_abrt_found = true;
			if (le->type & LE_START) {
				trans.start_addr = le;
				start_found = true;
				break;
			}
		}
		if (tail == 0 || tail == head)
		    break;
		/* prev log entry */
		le--;
		/* Handle uncommitted transactions */
		if ((gen_id == le16_to_cpu(le->gen_id))
			&& (le->type & LE_COMMIT || le->type & LE_ABORT)) {
			BUG_ON(trans.transaction_id == 
				le32_to_cpu(le->transaction_id));
			le++;
			break;
		}
		tail = prev_log_entry(sbi->jsize, tail);
	} while (1);

	if (start_found && !cmt_or_abrt_found)
		flatfs_undo_transaction(sb, &trans);

	if (gen_id == MAX_GEN_ID) {
		if (!start_found)
			trans.start_addr = le;
		/* make sure the changes made by flatfs_undo_transaction() are
		 * persistent before invalidating the log entries */
		if (start_found && !cmt_or_abrt_found) {
			PERSISTENT_MARK();
			PERSISTENT_BARRIER();
		}
		flatfs_invalidate_logentries(sb, &trans);
	}
	return tail;
}

/* process the transaction starting at a valid log entry *le */
/* called by the log cleaner and journal recovery */
static uint32_t flatfs_process_transaction(struct super_block *sb, uint32_t head,
	uint32_t tail, flatfs_logentry_t *le, bool recover, int *processed)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_transaction_t trans;
	uint16_t gen_id;
	uint32_t new_head = head;
	int handled = 0;

	*processed = 0;
	gen_id = le16_to_cpu(le->gen_id);
	if (!(le->type & LE_START)) {
		flatfs_dbg("start of trans %x but LE_START not set. gen_id %d\n",
				le32_to_cpu(le->transaction_id), gen_id);
		return next_log_entry(sbi->jsize, new_head);
	}
	memset(&trans, 0, sizeof(trans));
	trans.transaction_id = le32_to_cpu(le->transaction_id);
	trans.start_addr = le;
	trans.gen_id = gen_id;
	do {
		trans.num_entries++;
		trans.num_used++;
		new_head = next_log_entry(sbi->jsize, new_head);
		handled++;

		/* Handle committed/aborted transactions */
		if ((gen_id == le16_to_cpu(le->gen_id)) && (le->type & LE_COMMIT
					|| le->type & LE_ABORT)) {
			head = new_head;
			if ((le->type & LE_COMMIT) && sbi->redo_log)
				flatfs_redo_transaction(sb, &trans, recover);

			if (gen_id == MAX_GEN_ID) {
				if ((le->type & LE_COMMIT) && sbi->redo_log) {
					PERSISTENT_MARK();
					PERSISTENT_BARRIER();
				}
				flatfs_invalidate_logentries(sb, &trans);
			}
			break;
		}
		/* next log entry */
		le++;
		/* Handle uncommitted transactions */
		if ((new_head == tail) || ((gen_id == le16_to_cpu(le->gen_id))
			    && (le->type & LE_START))) {
			/* found a new valid transaction w/o finding a commit */
			if (recover) {
				/* if this function is called by recovery, move
				 * ahead even if we didn't find a commit record
				 * for this transaction */
				head = new_head;
				if (gen_id == MAX_GEN_ID)
					flatfs_invalidate_logentries(sb, &trans);
			}
			flatfs_dbg_trans("no cmt tid %d sa %p nle %d tail %x"
			" gen %d\n",
			trans.transaction_id,trans.start_addr,trans.num_entries,
			trans.num_used, trans.gen_id);
			/* dump_transaction(sbi, &trans); */
			break;
		}
	} while (new_head != tail);

	*processed = handled;
	return head;
}

static int flatfs_clean_journal(struct super_block *sb, bool unmount,
	int take_lock)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);
	uint32_t head;
	uint32_t new_head, tail;
	uint16_t gen_id;
	volatile __le64 *ptr_tail_genid;
	int processed = 0;
	int total = 0;
	u64 tail_genid;
	flatfs_logentry_t *le;

	if (take_lock)
		spin_lock(&sbi->journal_lock);
	head = le32_to_cpu(journal->head);
	ptr_tail_genid = (volatile __le64 *)&journal->tail;

	/* atomically read both tail and gen_id of journal. Normally use of
	 * volatile is prohibited in kernel code but since we use volatile
	 * to write to journal's tail and gen_id atomically, we thought we
	 * should use volatile to read them simultaneously and avoid locking
	 * them. */
	tail_genid = le64_to_cpu(*ptr_tail_genid);
	tail = tail_genid & 0xFFFFFFFF;
	gen_id = (tail_genid >> 32) & 0xFFFF;

	/* journal wraparound happened. so head points to prev generation id */
	if (tail < head)
		gen_id = prev_gen_id(gen_id);
	flatfs_dbg_trans("starting journal cleaning %x %x\n", head, tail);
	while (head != tail) {
		le = (flatfs_logentry_t *)(sbi->journal_base_addr + head);
		if (gen_id == le16_to_cpu(le->gen_id)) {
			/* found a valid log entry, process the transaction */
			new_head = flatfs_process_transaction(sb, head, tail,
				le, false, &processed);
			total += processed;
			/* no progress was made. return */
			if (new_head == head)
				break;
			head = new_head;
		} else {
			if (gen_id == MAX_GEN_ID) {
				flatfs_memunlock_range(sb, le, sizeof(*le));
				invalidate_gen_id(le);
				flatfs_memlock_range(sb, le, sizeof(*le));
			}
			head = next_log_entry(sbi->jsize, head);
		}
		/* handle journal wraparound */
		if (head == 0)
			gen_id = next_gen_id(gen_id);
	}
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	flatfs_memunlock_range(sb, journal, sizeof(*journal));
	journal->head = cpu_to_le32(head);
	flatfs_memlock_range(sb, journal, sizeof(*journal));
	flatfs_flush_buffer(&journal->head, sizeof(journal->head), true);
	if (unmount) {
		PERSISTENT_MARK();
		if (journal->head != journal->tail)
			flatfs_dbg("FLATFS: umount but journal not empty %x:%x\n",
			le32_to_cpu(journal->head), le32_to_cpu(journal->tail));
		PERSISTENT_BARRIER();
	}
	flatfs_dbg_trans("leaving journal cleaning %x %x\n", head, tail);
	if (take_lock)
		spin_unlock(&sbi->journal_lock);
	return total;
}

static void log_cleaner_try_sleeping(struct  flatfs_sb_info *sbi)
{
	DEFINE_WAIT(wait);
	prepare_to_wait(&sbi->log_cleaner_wait, &wait, TASK_INTERRUPTIBLE);
	schedule();
	finish_wait(&sbi->log_cleaner_wait, &wait);
}

static int flatfs_log_cleaner(void *arg)
{
	struct super_block *sb = (struct super_block *)arg;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	flatfs_dbg_trans("Running log cleaner thread\n");
	for ( ; ; ) {
		log_cleaner_try_sleeping(sbi);

		if (kthread_should_stop())
			break;

		flatfs_clean_journal(sb, false, 1);
	}
	flatfs_clean_journal(sb, true, 1);
	flatfs_dbg_trans("Exiting log cleaner thread\n");
	return 0;
}

static int flatfs_journal_cleaner_run(struct super_block *sb)
{
	int ret = 0;
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	init_waitqueue_head(&sbi->log_cleaner_wait);

	sbi->log_cleaner_thread = kthread_run(flatfs_log_cleaner, sb,
			"flatfs_log_cleaner_0x%llx", sbi->phys_addr);
	if (IS_ERR(sbi->log_cleaner_thread)) {
		/* failure at boot is fatal */
		flatfs_err(sb, "Failed to start flatfs log cleaner thread\n");
		ret = -1;
	}
	return ret;
}

int flatfs_journal_soft_init(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);

	sbi->next_transaction_id = 0;
	sbi->journal_base_addr = flatfs_get_block(sb,le64_to_cpu(journal->base));
	sbi->jsize = le32_to_cpu(journal->size);
	spin_lock_init(&sbi->journal_lock);
	sbi->redo_log = !!le16_to_cpu(journal->redo_logging);

	return flatfs_journal_cleaner_run(sb);
}

int flatfs_journal_hard_init(struct super_block *sb, uint64_t base,
	uint32_t size)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);

	flatfs_memunlock_range(sb, journal, sizeof(*journal));
	journal->base = cpu_to_le64(base);
	journal->size = cpu_to_le32(size);
	journal->gen_id = cpu_to_le16(1);
	journal->head = journal->tail = 0;
	/* lets do Undo logging for now */
	journal->redo_logging = 0;
	flatfs_memlock_range(sb, journal, sizeof(*journal));

	sbi->journal_base_addr = flatfs_get_block(sb, base);
	flatfs_memunlock_range(sb, sbi->journal_base_addr, size);
	memset_nt(sbi->journal_base_addr, 0, size);
	flatfs_memlock_range(sb, sbi->journal_base_addr, size);

	return flatfs_journal_soft_init(sb);
}

static void wakeup_log_cleaner(struct flatfs_sb_info *sbi)
{
	if (!waitqueue_active(&sbi->log_cleaner_wait))
		return;
	flatfs_dbg_trans("waking up the cleaner thread\n");
	wake_up_interruptible(&sbi->log_cleaner_wait);
}

int flatfs_journal_uninit(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	if (sbi->log_cleaner_thread)
		kthread_stop(sbi->log_cleaner_thread);
	return 0;
}

inline flatfs_transaction_t *flatfs_current_transaction(void)
{
	return (flatfs_transaction_t *)current->journal_info;
}

static int flatfs_free_logentries(struct super_block *sb, int max_log_entries)
{
	int freed_entries = 0;

	freed_entries = flatfs_clean_journal(sb, false, 0);
	return LOGENTRY_SIZE * freed_entries;
}

void *flatfs_init_transaction(struct super_block *sb, flatfs_transaction_t *trans,
                              int max_log_entries) {
	flatfs_journal_t *journal = flatfs_get_journal(sb);
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	uint32_t head, tail, req_size, avail_size, freed_size;
	uint64_t base;
	int retry = 0;

	/* If it is an undo log, need one more log-entry for commit record */
	if (!sbi->redo_log)
		max_log_entries++;

	trans->num_used = 0;
	trans->num_entries = max_log_entries;
	trans->t_journal = journal;
	req_size = max_log_entries << LESIZE_SHIFT;

	spin_lock(&sbi->journal_lock);

	tail = le32_to_cpu(journal->tail);
	head = le32_to_cpu(journal->head);
	trans->transaction_id = sbi->next_transaction_id++;
again:
	trans->gen_id = le16_to_cpu(journal->gen_id);
	avail_size = (tail >= head) ?
		(sbi->jsize - (tail - head)) : (head - tail);
	avail_size = avail_size - LOGENTRY_SIZE;

	if (avail_size < req_size) {
		/* run the log cleaner function to free some log entries */
		freed_size = 0;
		for (retry = 0; retry < 3; retry++) {
			freed_size += flatfs_free_logentries(sb,
						max_log_entries);
			if ((avail_size + freed_size) >= req_size)
				break;
		}

		if ((avail_size + freed_size) < req_size)
			goto journal_full;
	}
	base = le64_to_cpu(journal->base) + tail;
	tail = tail + req_size;
	/* journal wraparound because of this transaction allocation.
	 * start the transaction from the beginning of the journal so
	 * that we don't have any wraparound within a transaction */
	flatfs_memunlock_range(sb, journal, sizeof(*journal));
	if (tail >= sbi->jsize) {
		u64 *ptr;
		tail = 0;
		ptr = (u64 *)&journal->tail;
		/* writing 8-bytes atomically setting tail to 0 */
		set_64bit(ptr, (__force u64)cpu_to_le64((u64)next_gen_id(
					le16_to_cpu(journal->gen_id)) << 32));
		flatfs_memlock_range(sb, journal, sizeof(*journal));
		flatfs_dbg_trans("journal wrapped. tail %x gid %d cur tid %d\n",
			le32_to_cpu(journal->tail),le16_to_cpu(journal->gen_id),
				sbi->next_transaction_id - 1);
		goto again;
	} else {
		journal->tail = cpu_to_le32(tail);
		flatfs_memlock_range(sb, journal, sizeof(*journal));
	}
	flatfs_flush_buffer(&journal->tail, sizeof(u64), false);
	spin_unlock(&sbi->journal_lock);

	avail_size = avail_size - req_size;
	/* wake up the log cleaner if required */
	if ((sbi->jsize - avail_size) > (sbi->jsize >> 3))
		wakeup_log_cleaner(sbi);

	flatfs_dbg_trans("new transaction tid %d nle %d avl sz %x sa %llx\n",
		trans->transaction_id, max_log_entries, avail_size, base);
	trans->start_addr = flatfs_get_block(sb, base);

    if (!trans->on_stack) {
        trans->info_start_addr = kmalloc(sizeof(flatfs_le_info_t) * max_log_entries, GFP_ATOMIC);
    }

	trans->parent = (flatfs_transaction_t *)current->journal_info;
	current->journal_info = trans;
	return trans;
journal_full:
	spin_unlock(&sbi->journal_lock);
	flatfs_err(sb, "Journal full. base %llx sz %x head:tail %x:%x ncl %x\n",
		le64_to_cpu(journal->base), le32_to_cpu(journal->size),
		le32_to_cpu(journal->head), le32_to_cpu(journal->tail),
		max_log_entries);
	flatfs_err(sb, "avail size %u, freed size %u, request size %u\n",
		avail_size, freed_size, req_size);
	flatfs_free_transaction(trans);
	return ERR_PTR(-EAGAIN);
}

flatfs_transaction_t *flatfs_new_transaction(struct super_block *sb,
		int max_log_entries)
{
    flatfs_transaction_t *trans;
	timing_t log_time;

#if 0
	trans = flatfs_current_transaction();

	if (trans) {
		BUG_ON(trans->t_journal != journal);
		return trans;
	}
#endif

	FLATFS_START_TIMING(new_trans_t, log_time);

	trans = flatfs_alloc_transaction();
	if (!trans)
		return ERR_PTR(-ENOMEM);
	memset(trans, 0, sizeof(*trans));
    trans = flatfs_init_transaction(sb, trans, max_log_entries);

	FLATFS_END_TIMING(new_trans_t, log_time);

    return trans;
}

static inline void flatfs_commit_logentry(struct super_block *sb,
		flatfs_transaction_t *trans, flatfs_logentry_t *le)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	if (sbi->redo_log) {
		/* Redo Log */
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
		/* Atomically write the commit type */
		le->type |= LE_COMMIT;
		barrier();
		/* Atomically make the log entry valid */
		le->gen_id = cpu_to_le16(trans->gen_id);
		flatfs_flush_buffer(le, LOGENTRY_SIZE, false);
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
		/* Update the FS in place */
		flatfs_flush_transaction(sb, trans);
	} else {
		/* Undo Log */
		/* Update the FS in place: currently already done. so
		 * only need to clflush */
		flatfs_flush_transaction(sb, trans);
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
		/* Atomically write the commit type */
		le->type |= LE_COMMIT;
		barrier();
		/* Atomically make the log entry valid */
		le->gen_id = cpu_to_le16(trans->gen_id);
		flatfs_flush_buffer(le, LOGENTRY_SIZE, true);
	}
}

int flatfs_add_logentry(struct super_block *sb,
		flatfs_transaction_t *trans, void *addr, uint16_t size, u8 type)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_logentry_t *le;
    flatfs_le_info_t *lei;
	int num_les = 0, i;
	uint64_t le_start = size ? flatfs_get_addr_off(sbi, addr) : 0;
	uint8_t le_size;
	timing_t add_log_time;

	if (trans == NULL)
		return -EINVAL;

	FLATFS_START_TIMING(add_log_t, add_log_time);
	le = trans->start_addr + trans->num_used;
    lei = trans->info_start_addr + trans->num_used;

	if (size == 0) {
		/* At least one log entry required for commit/abort log entry */
		if ((type & LE_COMMIT) || (type & LE_ABORT))
			num_les = 1;
	} else
		num_les = (size + sizeof(le->data) - 1)/sizeof(le->data);

	flatfs_dbg_trans("add le id %d size %x, num_les %d tail %x le %p\n",
		trans->transaction_id, size, trans->num_entries,
		trans->num_used, le);

	if ((trans->num_used + num_les) > trans->num_entries) {
		flatfs_err(sb, "Log Entry full. tid %x ne %x tail %x size %x\n",
			trans->transaction_id, trans->num_entries,
			trans->num_used, size);
		dump_transaction(sbi, trans);
		dump_stack();
		return -ENOMEM;
	}

	flatfs_memunlock_range(sb, le, sizeof(*le) * num_les);
	for (i = 0; i < num_les; i++) {
		le->addr_offset = cpu_to_le64(le_start);
		le->transaction_id = cpu_to_le32(trans->transaction_id);
		le_size = (i == (num_les - 1)) ? size : sizeof(le->data);
		le->size = le_size;
		size -= le_size;
		if (le_size)
			memcpy(le->data, addr, le_size);
		le->type = type;

        if (!trans->on_stack) {
            lei->ptr = addr;
            lei->size = le_size;
        }

		if (i == 0 && trans->num_used == 0)
			le->type |= LE_START;
		trans->num_used++;

		/* handle special log entry */
		if (i == (num_les - 1) && (type & LE_COMMIT)) {
			flatfs_commit_logentry(sb, trans, le);
			flatfs_memlock_range(sb, le, sizeof(*le) * num_les);
			FLATFS_END_TIMING(add_log_t, add_log_time);
			return 0;
		}
		/* put a compile time barrier so that compiler doesn't reorder
		 * the writes to the log entry */
		barrier();

		/* Atomically make the log entry valid */
		le->gen_id = cpu_to_le16(trans->gen_id);
		flatfs_flush_buffer(le, LOGENTRY_SIZE, false);

		addr += le_size;
		le_start += le_size;
		le++;
        lei++;
	}
	flatfs_memlock_range(sb, le, sizeof(*le) * num_les);
	if (!sbi->redo_log) {
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
	}
	FLATFS_END_TIMING(add_log_t, add_log_time);
	return 0;
}

int flatfs_commit_transaction(struct super_block *sb,
		flatfs_transaction_t *trans)
{
	timing_t commit_time;

	if (trans == NULL)
		return 0;
	/* Add the commit log-entry */
	flatfs_add_logentry(sb, trans, NULL, 0, LE_COMMIT);

	FLATFS_START_TIMING(commit_trans_t, commit_time);
	flatfs_dbg_trans("completing transaction for id %d\n",
		trans->transaction_id);

	current->journal_info = trans->parent;
	flatfs_free_transaction(trans);
	FLATFS_END_TIMING(commit_trans_t, commit_time);
	return 0;
}

int flatfs_abort_transaction(struct super_block *sb, flatfs_transaction_t *trans)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);

	if (trans == NULL)
		return 0;
	flatfs_dbg_trans("abort trans for tid %x sa %p numle %d tail %x gen %d\n",
		trans->transaction_id, trans->start_addr, trans->num_entries,
		trans->num_used, trans->gen_id);
	dump_transaction(sbi, trans);
	/*dump_stack();*/

	if (!sbi->redo_log) {
		/* Undo Log */
		flatfs_undo_transaction(sb, trans);
		PERSISTENT_MARK();
		PERSISTENT_BARRIER();
	}
	/* add a abort log entry */
	flatfs_add_logentry(sb, trans, NULL, 0, LE_ABORT);
	current->journal_info = trans->parent;
	flatfs_free_transaction(trans);
	return 0;
}

static void invalidate_remaining_journal(struct super_block *sb,
	void *journal_vaddr, uint32_t jtail, uint32_t jsize)
{
	flatfs_logentry_t *le = (flatfs_logentry_t *)(journal_vaddr + jtail);
	void *start = le;

	flatfs_memunlock_range(sb, start, jsize - jtail);
	while (jtail < jsize) {
		invalidate_gen_id(le);
		le++;
		jtail += LOGENTRY_SIZE;
	}
	flatfs_memlock_range(sb, start, jsize - jtail);
}

/* we need to increase the gen_id to invalidate all the journal log
 * entries. This is because after the recovery, we may still have some
 * valid log entries beyond the tail (before power failure, they became
 * persistent before the journal tail could become persistent.
 * should gen_id and head be updated atomically? not necessarily? we
 * can update gen_id before journal head because gen_id and head are in
 * the same cacheline */
static void flatfs_forward_journal(struct super_block *sb, struct flatfs_sb_info
		*sbi, flatfs_journal_t *journal)
{
	uint16_t gen_id = le16_to_cpu(journal->gen_id);
	/* handle gen_id wrap around */
	if (gen_id == MAX_GEN_ID) {
		invalidate_remaining_journal(sb, sbi->journal_base_addr,
			le32_to_cpu(journal->tail), sbi->jsize);
	}
	PERSISTENT_MARK();
	gen_id = next_gen_id(gen_id);
	/* make all changes persistent before advancing gen_id and head */
	PERSISTENT_BARRIER();
	flatfs_memunlock_range(sb, journal, sizeof(*journal));
	journal->gen_id = cpu_to_le16(gen_id);
	barrier();
	journal->head = journal->tail;
	flatfs_memlock_range(sb, journal, sizeof(*journal));
	flatfs_flush_buffer(journal, sizeof(*journal), false);
}

static int flatfs_recover_undo_journal(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);
	uint32_t tail = le32_to_cpu(journal->tail);
	uint32_t head = le32_to_cpu(journal->head);
	uint16_t gen_id = le16_to_cpu(journal->gen_id);
	flatfs_logentry_t *le;

	while (head != tail) {
		/* handle journal wraparound */
		if (tail == 0)
			gen_id = prev_gen_id(gen_id);
		tail = prev_log_entry(sbi->jsize, tail);

		le = (flatfs_logentry_t *)(sbi->journal_base_addr + tail);
		if (gen_id == le16_to_cpu(le->gen_id)) {
			tail = flatfs_recover_transaction(sb, head, tail, le);
		} else {
			if (gen_id == MAX_GEN_ID) {
				flatfs_memunlock_range(sb, le, sizeof(*le));
				invalidate_gen_id(le);
				flatfs_memlock_range(sb, le, sizeof(*le));
			}
		}
	}
	flatfs_forward_journal(sb, sbi, journal);
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	return 0;
}

static int flatfs_recover_redo_journal(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);
	uint32_t tail = le32_to_cpu(journal->tail);
	uint32_t head = le32_to_cpu(journal->head);
	uint16_t gen_id = le16_to_cpu(journal->gen_id);
	int processed = 0;
	flatfs_logentry_t *le;

	/* journal wrapped around. so head points to previous generation id */
	if (tail < head)
		gen_id = prev_gen_id(gen_id);

	while (head != tail) {
		le = (flatfs_logentry_t *)(sbi->journal_base_addr + head);
		if (gen_id == le16_to_cpu(le->gen_id)) {
			head = flatfs_process_transaction(sb, head, tail,
				le, true, &processed);
		} else {
			if (gen_id == MAX_GEN_ID) {
				flatfs_memunlock_range(sb, le, sizeof(*le));
				invalidate_gen_id(le);
				flatfs_memlock_range(sb, le, sizeof(*le));
			}
			head = next_log_entry(sbi->jsize, head);
		}
		/* handle journal wraparound */
		if (head == 0)
			gen_id = next_gen_id(gen_id);
	}
	flatfs_forward_journal(sb, sbi, journal);
	PERSISTENT_MARK();
	PERSISTENT_BARRIER();
	return 0;
}

int flatfs_recover_journal(struct super_block *sb)
{
	struct flatfs_sb_info *sbi = FLATFS_SB(sb);
	flatfs_journal_t *journal = flatfs_get_journal(sb);
	uint32_t tail = le32_to_cpu(journal->tail);
	uint32_t head = le32_to_cpu(journal->head);
	uint16_t gen_id = le16_to_cpu(journal->gen_id);

	/* is the journal empty? true if unmounted properly. */
	if (head == tail)
		return 0;
	flatfs_dbg("FLATFS: journal recovery. head:tail %x:%x gen_id %d\n",
		head, tail, gen_id);
	if (sbi->redo_log)
		flatfs_recover_redo_journal(sb);
	else
		flatfs_recover_undo_journal(sb);
	return 0;
}

