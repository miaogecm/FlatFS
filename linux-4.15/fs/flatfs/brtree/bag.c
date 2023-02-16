/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * BrBag records each BrTree operation to a single file, which can be replayed
 * later in user-space, to facilitate BrTree debugging and performance tunning.
 */

#include <linux/fastr.h>
#include "../flatfs.h"
#include "tree.h"
#include "bag.h"

#define BRBAG_SIZE      (1024 * 1024 * 1024 * 1ul)
#define BRBAG_DUMP_PATH "/etc/brbag"

static char *bag = NULL;
static atomic64_t off = ATOMIC64_INIT(0);

void brbag_init(void) {
    if (unlikely(!bag)) {
        bag = vmalloc(BRBAG_SIZE);
        BUG_ON(!bag);
    }
}

void brbag_reset(void) {
    atomic64_set(&off, 0);
}

static void append(char *buf, long len) {
    long p = atomic64_fetch_add(len, &off);
    memcpy(&bag[p], buf, len);
}

void brbag_op(brbag_lbuf_t *lb, const char *opname, brt_tree_t *tree) {
    lb->p = lb->buf;
    /* opname pid tree */
    lb->p += sprintf(lb->p, "%s %d %lx ",
                     opname, pid_nr(task_pid(current)), (unsigned long) tree);
}

void brbag_fastr(brbag_lbuf_t *lb, fastr_t fs) {
    /* length string */
    lb->p += sprintf(lb->p, "%lu %.*s ", fs.len, FASTR_FMT(fs));
}

void brbag_value(brbag_lbuf_t *lb, brt_val_t val) {
    /* value */
    lb->p += sprintf(lb->p, "%lu ", val);
}

void brbag_ppc(brbag_lbuf_t *lb, ppc_t ppc) {
    /* ppc */
    lb->p += sprintf(lb->p, "%lu ", ppc);
}

void brbag_tree(brbag_lbuf_t *lb, brt_tree_t *tree) {
    /* tree */
    lb->p += sprintf(lb->p, "%lx ", (unsigned long) tree);
}

void brbag_chgpre_opt(brbag_lbuf_t *lb, brt_chgpre_opt_t *opt) {
    /* dst src pppcs pppc */
    brbag_fastr(lb, opt->dst);
    brbag_fastr(lb, opt->src);
    brbag_ppcs(lb, opt->pppcs);
    brbag_ppc(lb, opt->pppc);
}

void brbag_ppcs(brbag_lbuf_t *lb, ppcs_t ppcs) {
    /* nppc ppc1 ppc2 ppc3 ... ppcn */
    size_t i, n_ppc = ppcs_n_ppc(ppcs);
    lb->p += sprintf(lb->p, "%lu ", n_ppc);
    for (i = 0; i < n_ppc; i++) {
        brbag_ppc(lb, ppcs.words[i]);
    }
}

void brbag_end(brbag_lbuf_t *lb) {
#ifdef CONFIG_FLATFS_DEBUG
    printk("brbag: append %s\n", lb->buf);
#endif
    /* \n */
    lb->p += sprintf(lb->p, "\n");
    append(lb->buf, lb->p - lb->buf);
}

void brbag_dump(void) {
    mm_segment_t old_fs;
    struct file *f = filp_open(BRBAG_DUMP_PATH, O_RDWR | O_CREAT, 0644);
    BUG_ON(IS_ERR(f));
    old_fs = get_fs();
    set_fs(KERNEL_DS);
    vfs_write(f, bag, atomic64_read(&off), &f->f_pos);
    set_fs(old_fs);
    filp_close(f, NULL);
}
