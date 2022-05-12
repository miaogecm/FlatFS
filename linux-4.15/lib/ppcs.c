/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Manipulate PPCs
 * PPCs: PPC string / plural form of PPC
 */

#include <linux/ppcs.h>

void ppcs_append(ppcs_t *dst, ppcs_t src, size_t off, size_t n_level) {
    size_t depth_fix, i, cur;
    ppc_t first_ppc;
    ppc_t *ending = &dst->words[ppcs_n_ppc(*dst) - 1];

    if (unlikely(!n_level)) {
        return;
    }

    depth_fix = ppcs_shrink_pre_oop(&src, off);
    depth_fix = min(n_level, depth_fix);
    first_ppc = ppcs_get_ppc(src, 0);
    ppc_set_dep(&first_ppc, depth_fix);

    if (!ppcs_is_empty(*dst) && ppc_may_merge(*ending, first_ppc)) {
        ppc_set_dep(ending, ppc_get_dep(*ending) + depth_fix);
    } else {
        *++ending = first_ppc;
    }
    src.words++;
    src.len -= sizeof(ppc_t);
    n_level -= depth_fix;

    for (cur = 0, i = 0; cur < n_level; cur += ppc_get_dep(ppcs_get_ppc(src, i)), i++) {
        *++ending = ppcs_get_ppc(src, i);
    }
    ppc_set_dep(ending, ppc_get_dep(*ending) - (cur - n_level));
    dst->len = (++ending - dst->words) * sizeof(ppc_t);
}

void ppcs_prepend(ppcs_t *dst, ppcs_t src, size_t off, size_t n_level) {
    size_t depth_fix, i, cur;
    ppc_t last_ppc;
    ppc_t *beginning = &dst->words[0];

    if (unlikely(!n_level)) {
        return;
    }

    depth_fix = ppcs_shrink_suf_oop(&src, off);
    depth_fix = min(n_level, depth_fix);
    last_ppc = ppcs_get_ppc(src, ppcs_n_ppc(src) - 1);
    ppc_set_dep(&last_ppc, depth_fix);

    if (!ppcs_is_empty(*dst) && ppc_may_merge(*beginning, last_ppc)) {
        ppc_set_dep(beginning, ppc_get_dep(*beginning) + depth_fix);
    } else {
        *--beginning = last_ppc;
    }
    src.len -= sizeof(ppc_t);
    n_level -= depth_fix;

    for (cur = 0, i = ppcs_n_ppc(src) - 1; cur < n_level; cur += ppc_get_dep(ppcs_get_ppc(src, i)), i--) {
        *--beginning = ppcs_get_ppc(src, i);
    }
    ppc_set_dep(beginning, ppc_get_dep(*beginning) - (cur - n_level));
    dst->len += (dst->words - beginning) * sizeof(ppc_t);
    dst->words = beginning;
}
