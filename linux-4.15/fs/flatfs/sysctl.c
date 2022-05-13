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
#include "ndcache.h"
#include "brbag.h"

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
