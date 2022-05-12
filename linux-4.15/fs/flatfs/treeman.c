/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */
#include <linux/flatfs_define.h>
#include <linux/list.h>

#include "brtree.h"
#include "treeman.h"

static int num_blk_type[FLATFS_BLOCK_TYPE_MAX] = {1, 512, 262144};

static void init_node_block_free_list(struct super_block* sb, struct list_head* free_list, int cpu, unsigned long blocknr, int type) {
	brt_nodeblk_hdr_t * header;

	header = brt_nodeblk_hdr_of(
            (void *) flatfs_get_block(sb, flatfs_get_block_off(sb, blocknr, FLATFS_BLOCK_TYPE_4K)));
	header->type = type;
    header->cpu = cpu;
    header->bitmap = 0;
    header->blocknr = blocknr;
    INIT_LIST_HEAD(&header->list);
    list_add(&header->list, free_list);

	flatfs_flush_buffer(&header, sizeof(brt_nodeblk_hdr_t), false);
}

static void alloc_node_block_free_list(struct super_block* sb, struct list_head* free_list, int cpu, int type) {
    unsigned long blocknr;
    int num_blks = (BRT_N_NODE_BLOCK * PAGE_SIZE) >> PAGE_SHIFT_2M;
	int num_pages = (1 << PAGE_SHIFT_2M) >> PAGE_SHIFT, i;
	int cnt;

    for (cnt = 0; cnt < num_blks; cnt ++) {
        flatfs_new_block(sb, &blocknr, FLATFS_BLOCK_TYPE_2M, 0);
		for (i = 0; i < num_pages; i ++, blocknr ++) {
			init_node_block_free_list(sb, free_list, cpu, blocknr, type);
		}
    }
}

static void init_entry_block_free_list(struct super_block* sb, struct list_head* free_list, int cpu, unsigned long blocknr) {
	brt_entblk_hdr_t * header;

	header = brt_entblk_hdr_of(sb, blocknr);
    header->num_entries = 0;
    header->in_use = 0;
    header->cpu = cpu;
    header->blocknr = blocknr;
    header->lock = kmalloc(sizeof(spinlock_t), GFP_ATOMIC);
    spin_lock_init(header->lock);
    INIT_LIST_HEAD(&header->list);
    list_add(&header->list, free_list);

	flatfs_flush_buffer(&header, sizeof(brt_entblk_hdr_t), false);
}

static void alloc_entry_block_free_list(struct super_block* sb, struct list_head* free_list, int cpu) {
    unsigned long blocknr;
	int num_blks = (BRT_N_ENT_BLOCK * PAGE_SIZE) >> PAGE_SHIFT_2M;
	int num_pages = (1 << PAGE_SHIFT_2M) >> PAGE_SHIFT, i;
    int cnt;

    for (cnt = 0; cnt < num_blks; cnt ++) {
        flatfs_new_block(sb, &blocknr, FLATFS_BLOCK_TYPE_2M, 0);
		for (i = 0; i < num_pages; i ++, blocknr ++) {
			init_entry_block_free_list(sb, free_list, cpu, blocknr);
		}
    }
}

static inline void init_bplus_tree_node(brt_tree_t *tree, brt_node_t *node, u8 type, u8 index) {
    struct super_block *sb = tree->sbi->super;
    size_t pre_buffer_size = 0;

    memset(node, 0, sizeof(*node));

    node->type = type;
    node->index = index;
    node->next = BRT_NOFF;
    node->blocknr = BRT_NOFF;
    node->refcount = 1;
    // TODO: Make prefix size flexible
    /* for pathname */
    pre_buffer_size += BRT_NDPRE_INIT_SIZE;
    /* for permission string */
    pre_buffer_size += BRT_NDPRE_INIT_SIZE;
    node->pre_buffer = flatfs_address_to_offset(sb, brt_entbuf_new(tree, node, (int) pre_buffer_size));
    node->path_pre_len = 0;
    // TODO: Support delta
    node->path_delta_len = 0;
    node->perm_pre_len = 0;
    node->dirtymap = 0;
    node->prestorer = BRT_NOFF;
    node->lock = kmalloc(sizeof(rwlock_t), GFP_ATOMIC);
    rwlock_init(node->lock);
}

void __free_bplus_tree_node(brt_tree_t *tree, brt_node_t *node) {
    struct flatfs_sb_info* sbi = tree->sbi;
    struct list_head *free_list;
    spinlock_t *free_list_lock;
    brt_nodeblk_hdr_t *header = brt_nodeblk_hdr_of(node);
    brt_entblk_hdr_t *entry_header = brt_entblk_hdr_of(sbi->super, node->blocknr);
    int cpu;
    brt_ndtype_t type = brt_nd_type(node);

    ENTER_BLOCK_MANIPULATION;

    cpu = le32_to_cpu(header->cpu);

    if (entry_header) {
        /*
         * When a node is freed, its entry header becomes a poor orphan. However, the orphan header may still contain
         * some entries, so we can not free the entry header immediately. But if we do so, when the num_entries becomes
         * zero, the entry header will never be able to be freed (because we only free full entries), thus resulting in
         * memory leak. The solution is to set entry->in_use to PAGE_SIZE.
         */
        spin_lock(entry_header->lock);
        entry_header->owner = BRT_NOFF;
        entry_header->in_use = PAGE_SIZE;
        spin_unlock(entry_header->lock);
    }

#ifdef BPLUSTREE_SUPPORT_CONCURRENCY
    kfree(node->latch);
#endif
    brt_entbuf_free(sbi->super, flatfs_offset_to_address(sbi->super, node->pre_buffer));

    if (type == BRT_LEAF) {
        free_list = &sbi->node_blk_lists[cpu]->free_leaf_node_lists;
        free_list_lock = &sbi->node_blk_lists[cpu]->free_leaf_node_list_locks;
    } else {
        free_list = &sbi->node_blk_lists[cpu]->free_internal_node_lists;
        free_list_lock = &sbi->node_blk_lists[cpu]->free_internal_node_list_locks;
    }

    spin_lock(free_list_lock);
    clear_bit_le(node->index, &header->bitmap);

	flatfs_flush_buffer(&header->bitmap, sizeof(__le64), false);

    if (hweight64(header->bitmap) == 0 && list_empty(&header->list)) {
        list_add(&header->list, free_list);
    }

    spin_unlock(free_list_lock);

    LEAVE_BLOCK_MANIPULATION;
}

brt_node_t *__alloc_bplus_tree_node(brt_tree_t *tree, brt_ndtype_t type)
{
    struct flatfs_sb_info* sbi = tree->sbi;
    struct super_block * sb = sbi->super;
    int cpu = smp_processor_id();
    struct list_head *free_list;
    spinlock_t *free_list_lock;
    brt_nodeblk_hdr_t *header, *tmp;
    brt_node_t *node;
    int node_per_block = (type == BRT_LEAF) ? BRT_LEAF_PER_BLK : BRT_INTN_PER_BLK;
    int node_size = (type == BRT_LEAF) ? BRT_LEAF_SIZE : BRT_INTN_SIZE;
    int index = 0;

    ENTER_BLOCK_MANIPULATION;

    if (type == BRT_LEAF) {
        free_list = &sbi->node_blk_lists[cpu]->free_leaf_node_lists;
        free_list_lock = &sbi->node_blk_lists[cpu]->free_leaf_node_list_locks;
    } else {
        free_list = &sbi->node_blk_lists[cpu]->free_internal_node_lists;
        free_list_lock = &sbi->node_blk_lists[cpu]->free_internal_node_list_locks;
    }

    spin_lock(free_list_lock);
    retry:
    list_for_each_entry_safe(header, tmp, free_list, list) {
        if (hweight64(header->bitmap) < node_per_block) {
            /* got a node */
            index = find_first_zero_bit_le(&header->bitmap, BITS_PER_LONG);
            set_bit_le(index, &header->bitmap);
            goto out;
        } else {
            /* remove it */
            list_del_init(&header->list);
        }
    }
    BUG(); // FIXME: sleep in atomic context
    alloc_node_block_free_list(sb, free_list, cpu, type);
    goto retry;

    out:
    spin_unlock(free_list_lock);

	flatfs_flush_buffer(&header->bitmap, sizeof(__le64), false);

    node = (brt_node_t *)(BRT_BLK_START(header) + index * node_size);
    init_bplus_tree_node(tree, node, type, index);

    LEAVE_BLOCK_MANIPULATION;

    return node;
}

static void free_entry_block(struct super_block* sb, brt_entblk_hdr_t * header) {
    struct list_head *free_list, *inuse_list;
    spinlock_t *free_list_lock, *inuse_list_lock;
    struct flatfs_sb_info* sbi = FLATFS_SB(sb);
    int cpu = header->cpu;

    inuse_list = &sbi->entry_blk_lists[cpu]->inuse_entry_block_lists;
    inuse_list_lock = &sbi->entry_blk_lists[cpu]->inuse_entry_block_list_locks;
    spin_lock(inuse_list_lock);
    list_del_init(&header->list);
    spin_unlock(inuse_list_lock);

    free_list = &sbi->entry_blk_lists[cpu]->free_entry_block_lists;
    free_list_lock = &sbi->entry_blk_lists[cpu]->free_entry_block_list_locks;
    spin_lock(free_list_lock);
    list_add_tail(&header->list, free_list);
    spin_unlock(free_list_lock);
}

static brt_entblk_hdr_t* alloc_entry_block(brt_tree_t* tree, brt_node_t* owner) {
    struct flatfs_sb_info* sbi = tree->sbi;
    struct super_block * sb = sbi->super;
    int cpu = smp_processor_id();
    brt_entblk_hdr_t *header, *tmp;
    struct list_head *free_list = &sbi->entry_blk_lists[cpu]->free_entry_block_lists;
    spinlock_t *free_list_lock = &sbi->entry_blk_lists[cpu]->free_entry_block_list_locks;
    struct list_head *inuse_list = &sbi->entry_blk_lists[cpu]->inuse_entry_block_lists;
    spinlock_t *inuse_list_lock = &sbi->entry_blk_lists[cpu]->inuse_entry_block_list_locks;

    spin_lock(free_list_lock);
    retry:
    list_for_each_entry_safe(header, tmp, free_list, list) {
        spin_lock(header->lock);
        if (header->num_entries == 0) {
            /* got a block */
            header->in_use = sizeof(brt_entblk_hdr_t);
            header->owner = flatfs_address_to_offset(sb, owner);
            owner->blocknr = header->blocknr;
            spin_unlock(header->lock);
            list_del_init(&header->list);
            goto out;
        }
        spin_unlock(header->lock);
    }
    BUG(); // FIXME: sleep in atomic context
    alloc_entry_block_free_list(sb, free_list, cpu);
    goto retry;

    out:
    spin_unlock(free_list_lock);

    spin_lock(inuse_list_lock);
    list_add(&header->list, inuse_list);
    spin_unlock(inuse_list_lock);

    return header;
}

void __free_bplus_entry_buffer(struct super_block *sb, void* entry) {
    brt_entblk_hdr_t *header;
    brt_node_t *owner;

    header = (brt_entblk_hdr_t*)BRT_BLK_START(entry);

    ENTER_BLOCK_MANIPULATION;

    spin_lock(header->lock);
    header->num_entries--;
    if (!header->num_entries && brt_entblk_full(header, 0)) {
        /* we can safely free this block */
        if (header->owner) {
            owner = (brt_node_t *)flatfs_offset_to_address(sb, header->owner);
            owner->blocknr = 0; // reset it to 0
        }
        //printk("free entry block %016lx %lu\n", header, header->blocknr);
        free_entry_block(sb, header);
    }
    spin_unlock(header->lock);

    LEAVE_BLOCK_MANIPULATION;
}

void* __alloc_bplus_entry_buffer(brt_tree_t *tree, brt_node_t *node, size_t esize) {
    brt_entblk_hdr_t *header;
    struct super_block* sb = tree->sbi->super;
    int old_inuse;

    esize = ALIGN(esize, 64);

    ENTER_BLOCK_MANIPULATION;

again:
    if (!node->blocknr) {
        /* allocate a new entry block */
        header = alloc_entry_block(tree, node);

        flatfs_flush_buffer(&node->blocknr, sizeof(__le64), false);
        spin_lock(header->lock);
    } else {
        header = brt_entblk_hdr_of(sb, node->blocknr);
        spin_lock(header->lock);
        if (!node->blocknr)
            goto again;

        if (brt_entblk_full(header, esize)) {
            header->in_use = PAGE_SIZE;
            spin_unlock(header->lock);

            /* allocate a new entry block */
            header = alloc_entry_block(tree, node);
            flatfs_flush_buffer(&node->blocknr, sizeof(__le64), false);
            spin_lock(header->lock);
        }
    }

    //printk("allocate a entry: %016lx entries %u inuse %u esize %d\n",  header, header->num_entries, header->in_use, esize);
    old_inuse = header->in_use;
    header->in_use += esize;
    header->num_entries ++;
    spin_unlock(header->lock);

    LEAVE_BLOCK_MANIPULATION;

    return (void*)(BRT_BLK_START(header) + old_inuse);
}

int deinit_node_block_list(struct flatfs_sb_info* sbi)
{
	int cpu;

    ENTER_BLOCK_MANIPULATION;

	for (cpu = 0; cpu < sbi->num_cpus; cpu ++)
    	kfree(sbi->node_blk_lists[cpu]);

    LEAVE_BLOCK_MANIPULATION;

    return 0;
}

int deinit_entry_block_list(struct flatfs_sb_info* sbi)
{
    struct list_head *free_list;
    spinlock_t *free_list_lock;
    struct list_head *inuse_list;
    spinlock_t *inuse_list_lock;
    brt_entblk_hdr_t *header, *tmp;
    int cpu;

    ENTER_BLOCK_MANIPULATION;

    for (cpu = 0; cpu < sbi->num_cpus; cpu ++) {
        free_list = &sbi->entry_blk_lists[cpu]->free_entry_block_lists;
        free_list_lock = &sbi->entry_blk_lists[cpu]->free_entry_block_list_locks;
        inuse_list = &sbi->entry_blk_lists[cpu]->inuse_entry_block_lists;
        inuse_list_lock = &sbi->entry_blk_lists[cpu]->inuse_entry_block_list_locks;

        spin_lock(free_list_lock);
        list_for_each_entry_safe(header, tmp, free_list, list) {
            kfree(header->lock);
            list_del(&header->list);
        }
        spin_unlock(free_list_lock);

        spin_lock(inuse_list_lock);
        list_for_each_entry_safe(header, tmp, inuse_list, list) {
            kfree(header->lock);
            list_del(&header->list);
        }
        spin_unlock(inuse_list_lock);

		kfree(sbi->entry_blk_lists[cpu]);
    }

    LEAVE_BLOCK_MANIPULATION;

    return 0;
}

/*
 * flatfs_allocate_tree_blocks: allocate blocks for node and entry block list
 */
static void flatfs_allocate_tree_blocks(struct super_block* sb, int type) {
	struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
	struct flatfs_block_item *item, *start;
	unsigned long total_size, num_blk_2M = 0, num_blk_1G = 0;
	int total_cpu = FLATFS_SB(sb)->num_cpus, i;
	unsigned long blocknr_1G, blocknr_2M, blocknr;

	flatfs_new_block(sb, &blocknr, FLATFS_BLOCK_TYPE_4K, 1);
	if (type) {
		flatfs_sb->s_node_blocknr = cpu_to_le64(blocknr);
		flatfs_flush_buffer(&flatfs_sb->s_node_blocknr, sizeof(__le64), false);
		total_size = BRT_N_NODE_BLOCK * PAGE_SIZE * total_cpu * 2;
	} else {
		flatfs_sb->s_entry_blocknr = cpu_to_le64(blocknr);
		flatfs_flush_buffer(&flatfs_sb->s_entry_blocknr, sizeof(__le64), false);
		total_size = BRT_N_ENT_BLOCK * PAGE_SIZE * total_cpu;
	}

	FLATFS_ASSERT(!(total_size % PAGE_SIZE_2M));
	start = item = flatfs_get_block(sb, flatfs_get_block_off(sb, blocknr, FLATFS_BLOCK_TYPE_4K));

	if (total_size >= PAGE_SIZE_1G) {
		num_blk_1G = total_size / PAGE_SIZE_1G;
		num_blk_2M = (total_size % PAGE_SIZE_1G) / PAGE_SIZE_2M;
	} else
		num_blk_2M = total_size / PAGE_SIZE_2M;

	printk("flatfs_allocate_tree_blocks: [%ld] 1G block, [%ld] 2M block\n", num_blk_1G, num_blk_2M);

    BEFORE_BLOCK_PREALLOCATION;

	for (i = 0; i < num_blk_1G; i ++, item ++) {
		flatfs_new_block(sb, &blocknr_1G, FLATFS_BLOCK_TYPE_1G, 0);
		item->i_type = FLATFS_BLOCK_TYPE_1G;
		item->i_blocknr = blocknr_1G;
	}

	for (i = 0; i < num_blk_2M; i ++, item ++) {
		flatfs_new_block(sb, &blocknr_2M, FLATFS_BLOCK_TYPE_2M, 0);
		item->i_type = FLATFS_BLOCK_TYPE_2M;
		item->i_blocknr = blocknr_2M;
	}

    AFTER_BLOCK_PREALLOCATION;

	flatfs_flush_buffer(start, PAGE_SIZE, false);
}

/*
 * init_node_block_lists: init a node block list during a new mount
 */
int init_node_block_lists(struct flatfs_sb_info* sbi)
{
    struct node_blk_list* list;
	struct super_block* sb = sbi->super;
	struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
	int total_cpu = sbi->num_cpus, cpu, i, cnt = 0;
	unsigned long blocknr;
	struct flatfs_block_item *item;

    ENTER_BLOCK_MANIPULATION;

	flatfs_allocate_tree_blocks(sb, 1);

	item = flatfs_get_block(sb, flatfs_get_block_off(sb, flatfs_sb->s_node_blocknr, FLATFS_BLOCK_TYPE_4K));
	blocknr = item->i_blocknr;
    for (cpu = 0; cpu < total_cpu; cpu ++) {
		list = kmalloc(sizeof(struct node_blk_list), GFP_ATOMIC);

        spin_lock_init(&list->free_leaf_node_list_locks);
        spin_lock_init(&list->free_internal_node_list_locks);
        INIT_LIST_HEAD(&list->free_leaf_node_lists);
        INIT_LIST_HEAD(&list->free_internal_node_lists);

		for (i = 0; i < BRT_N_NODE_BLOCK; i ++) {
        	init_node_block_free_list(sb, &list->free_leaf_node_lists, cpu, blocknr++, BRT_LEAF);
			init_node_block_free_list(sb, &list->free_internal_node_lists, cpu, blocknr++, BRT_INTN);

			if (cnt == (num_blk_type[item->i_type] - 2)) {
				item ++; cnt = 0;
				blocknr = item->i_blocknr;
			} else {
				cnt += 2;
			}
		}

		sbi->node_blk_lists[cpu] = list;
    }

    LEAVE_BLOCK_MANIPULATION;

    printk("init_node_block_lists\n");

    return 0;
}

/*
 * init_entry_block_lists: init an entry block list during a new mount
 */
int init_entry_block_lists(struct flatfs_sb_info* sbi)
{
    struct entry_blk_list* list;
    struct super_block* sb = sbi->super;
	struct flatfs_super_block *flatfs_sb = flatfs_get_super(sb);
    int total_cpu = sbi->num_cpus, cpu, i, cnt = 0;
   	unsigned long blocknr;
	struct flatfs_block_item *item;

    ENTER_BLOCK_MANIPULATION;

	flatfs_allocate_tree_blocks(sb, 0);

	item = flatfs_get_block(sb, flatfs_get_block_off(sb, flatfs_sb->s_entry_blocknr, FLATFS_BLOCK_TYPE_4K));
	blocknr = item->i_blocknr;

    for (cpu = 0; cpu < total_cpu; cpu ++) {
		list = kmalloc(sizeof(struct entry_blk_list), GFP_ATOMIC);

        spin_lock_init(&list->free_entry_block_list_locks);
        spin_lock_init(&list->inuse_entry_block_list_locks);
        INIT_LIST_HEAD(&list->free_entry_block_lists);
        INIT_LIST_HEAD(&list->inuse_entry_block_lists);

		for (i = 0; i < BRT_N_ENT_BLOCK; i ++) {
			init_entry_block_free_list(sb, &list->free_entry_block_lists, cpu, blocknr);

			if (cnt == (num_blk_type[item->i_type] - 1)) {
				item ++; cnt = 0;
				blocknr = item->i_blocknr;
			} else {
				cnt ++; blocknr++;
			}
		}

		sbi->entry_blk_lists[cpu] = list;
    }

    LEAVE_BLOCK_MANIPULATION;

    printk("init_entry_block_lists\n");

    return 0;
}
