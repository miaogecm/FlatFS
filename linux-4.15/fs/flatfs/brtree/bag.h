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

#ifndef _LINUX_BRBAG_H
#define _LINUX_BRBAG_H

#define BRBAG_LBUFSZ    256

typedef struct {
    char buf[BRBAG_LBUFSZ], *p;
} brbag_lbuf_t;

void brbag_init(void);
void brbag_reset(void);
void brbag_op(brbag_lbuf_t *lb, const char *opname, brt_tree_t *tree);
void brbag_fastr(brbag_lbuf_t *lb, fastr_t fs);
void brbag_value(brbag_lbuf_t *lb, brt_val_t val);
void brbag_ppc(brbag_lbuf_t *lb, ppc_t ppc);
void brbag_ppcs(brbag_lbuf_t *lb, ppcs_t ppcs);
void brbag_tree(brbag_lbuf_t *lb, brt_tree_t *tree);
void brbag_chgpre_opt(brbag_lbuf_t *lb, brt_chgpre_opt_t *opt);
void brbag_end(brbag_lbuf_t *lb);
void brbag_dump(void);

#endif //_LINUX_BRBAG_H
