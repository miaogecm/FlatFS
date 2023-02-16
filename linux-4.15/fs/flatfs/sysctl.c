/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Sysctl interface
 */

#include <linux/types.h>
#include <linux/linkage.h>
#include <linux/ctype.h>
#include <linux/fs.h>
#include <linux/sysctl.h>
#include <linux/module.h>
#include "flatfs.h"
#include "brtree/tree.h"
#include "brtree/ndcache.h"
#include "brtree/bag.h"

static char ndcache_statistics_buffer[256];

#ifdef CONFIG_NDCACHE_STATISTICS

static int ndcache_statistics_handler(struct ctl_table *ctl, int write,
                             void __user *buffer, size_t *lenp, loff_t *ppos) {
    u32 total, hit, miss, n_cmp;

    if (write) {
        atomic_set(&ndcache_stat.hit, 0);
        atomic_set(&ndcache_stat.miss, 0);
        atomic_set(&ndcache_stat.n_cmp, 0);
        return 0;
    }

    if (!*lenp || *ppos) {
        *lenp = 0;
        return 0;
    }

    hit = atomic_read(&ndcache_stat.hit);
    miss = atomic_read(&ndcache_stat.miss);
    n_cmp = atomic_read(&ndcache_stat.n_cmp);
    total = hit + miss;

    if (total > 0) {
        scnprintf(ndcache_statistics_buffer, sizeof(ndcache_statistics_buffer), "NDCache statistics:\n"
                "Hit/Miss/(Hit+Miss)=%u/%u/%u=%u%%/%u%%/100%%\n"
                "TotalNCompare=%u, AvgNCompare=%u/100\n",
                hit, miss, total, hit * 100 / total, miss * 100 / total,
                n_cmp, n_cmp * 100 / total);
    } else {
        scnprintf(ndcache_statistics_buffer, sizeof(ndcache_statistics_buffer), "No record available!\n");
    }

    return proc_dostring(ctl, write, buffer, lenp, ppos);
}

#else

static int ndcache_statistics_handler(struct ctl_table *ctl, int write,
                             void __user *buffer, size_t *lenp, loff_t *ppos)
{
	return 0;
}

#endif

static char brtree_statistics_buffer[PAGE_SIZE];
brt_tree_t *main_brtree;

static int brtree_statistics_handler(struct ctl_table *ctl, int write,
                                     void __user *buffer, size_t *lenp, loff_t *ppos) {
    brt_stat_t stat;
    brt_stat(main_brtree, &stat);
    scnprintf(brtree_statistics_buffer, sizeof(brtree_statistics_buffer),
              "BrTree statistics:\n"
              "height=%d\n"
              "nr_nodes=%d\n"
              "nr_entries=%d (avg. %d per node)\n"
              "total_filename_len=%lu (avg. %lu per entry)\n"
              "total_suf_len=%lu (avg. %lu per entry)\n"
              "total_suf_cached_len=%lu (avg. %lu per entry)\n"
              "total_path_len=%lu (avg. %lu per entry)\n"
              "nr_filename:        0s          1s          2s\n"
              "            0o      %08d    %08d    %08d\n"
              "            1o      %08d    %08d    %08d\n"
              "nr_suf:             0s          1s          2s\n"
              "            0o      %08d    %08d    %08d\n"
              "            1o      %08d    %08d    %08d\n"
              "nr_suf_cached:      0s          1s          2s\n"
              "            0o      %08d    %08d    %08d\n"
              "            1o      %08d    %08d    %08d\n",
              stat.height,
              stat.nr_nodes,
              stat.nr_entries, stat.nr_nodes ? stat.nr_entries / stat.nr_nodes : -1,
              stat.total_filename_len, stat.nr_entries ? stat.total_filename_len / stat.nr_entries : -1,
              stat.total_suf_len, stat.nr_entries ? stat.total_suf_len / stat.nr_entries : -1,
              stat.total_suf_cached_len, stat.nr_entries ? stat.total_suf_cached_len / stat.nr_entries : -1,
              stat.total_path_len, stat.nr_entries ? stat.total_path_len / stat.nr_entries : -1,
              stat.nr_filename[0][0], stat.nr_filename[1][0], stat.nr_filename[2][0],
              stat.nr_filename[0][1], stat.nr_filename[1][1], stat.nr_filename[2][1],
              stat.nr_suf[0][0], stat.nr_suf[1][0], stat.nr_suf[2][0],
              stat.nr_suf[0][1], stat.nr_suf[1][1], stat.nr_suf[2][1],
              stat.nr_suf_cached[0][0], stat.nr_suf_cached[1][0], stat.nr_suf_cached[2][0],
              stat.nr_suf_cached[0][1], stat.nr_suf_cached[1][1], stat.nr_suf_cached[2][1]);
    return proc_dostring(ctl, write, buffer, lenp, ppos);
}

static int brbag_handler(struct ctl_table *ctl, int write, void __user *buffer, size_t *lenp, loff_t *ppos) {
    if (write) {
        brbag_reset();
    } else {
        brbag_dump();
    }
    return 0;
}

static struct ctl_table_header *flatfs_callback_sysctl_table;

static struct ctl_table flatfs_cb_sysctls[] = {
        {
                .procname	= "ndcache_stat",
                .data		= ndcache_statistics_buffer,
                .maxlen		= sizeof(ndcache_statistics_buffer),
                .mode		= 0644,
                .proc_handler	= ndcache_statistics_handler,
        },
        {
                .procname	= "brbag",
                .mode		= 0644,
                .proc_handler = brbag_handler,
        },
        {
                .procname	= "brtree_stat",
                .data		= brtree_statistics_buffer,
                .maxlen		= sizeof(brtree_statistics_buffer),
                .mode		= 0644,
                .proc_handler	= brtree_statistics_handler,
        },
        { }
};

static struct ctl_table flatfs_cb_sysctl_dir[] = {
        {
                .procname = "flatfs",
                .mode = 0555,
                .child = flatfs_cb_sysctls,
        },
        { }
};

static struct ctl_table flatfs_cb_sysctl_root[] = {
        {
                .procname = "fs",
                .mode = 0555,
                .child = flatfs_cb_sysctl_dir,
        },
        { }
};

int flatfs_register_sysctl(void)
{
    flatfs_callback_sysctl_table = register_sysctl_table(flatfs_cb_sysctl_root);
    if (flatfs_callback_sysctl_table == NULL)
        return -ENOMEM;
    return 0;
}

void flatfs_unregister_sysctl(void)
{
    unregister_sysctl_table(flatfs_callback_sysctl_table);
    flatfs_callback_sysctl_table = NULL;
}
