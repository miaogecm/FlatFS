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

#ifndef _LINUX_PPCS_H
#define _LINUX_PPCS_H

#include <linux/fastr.h>
#include <linux/types.h>
#include <linux/uidgid.h>

#define PPCS_NULL            FASTR_NULL
#define PPCS_IS_NOT_NULL(p)  FASTR_IS_NOT_NULL(p)
#define PPCS_SIZEOF(nw)      FASTR_N_W2C(nw)
#define PPCS_LEN2NPPC(nc)    FASTR_N_C2W(nc)

/*
 * PPC-word layout:
 * (low)
 * uid[0:15]
 * gid[16:31]
 * u[32:32]
 * g[33:33]
 * o[34:34]
 * depth[35:63]
 * (high)
 */
typedef unsigned long ppc_t;
typedef fastr_t       ppcs_t;

static inline size_t ppc_get_dep(ppc_t ppc) {
    return ppc >> 35u;
}

/*
 * Can we merge two PPCs?
 * If and only if they share common uid/gid/ugo
 */
static inline int ppc_may_merge(ppc_t ppc1, ppc_t ppc2) {
    return (ppc1 << 29u) == (ppc2 << 29u);
}

static inline void ppc_set_dep(ppc_t *ppc, size_t dep) {
    *ppc = (*ppc & (~0ul >> 29u)) + (dep << 35u);
}

static inline size_t ppc_parse(ppc_t ppc,
                               u8 *u, u8 *g, u8 *o,
                               kuid_t *uid, kgid_t *gid) {
    uid->val = (ppc >>  0u) & 0xffffu;
    gid->val = (ppc >> 16u) & 0xffffu;
    *u       = (ppc >> 32u) & 0x01u;
    *g       = (ppc >> 33u) & 0x01u;
    *o       = (ppc >> 34u) & 0x01u;
    return     (ppc >> 35u);
}

static inline ppc_t ppc_generate(size_t depth,
                                 u8 u, u8 g, u8 o,
                                 kuid_t uid, kgid_t gid) {
    ppc_t reserved = depth << 35u,
                wo       = (unsigned long) (o != 0) << 34u,
                wg       = (unsigned long) (g != 0) << 33u,
                wu       = (unsigned long) (u != 0) << 32u,
                wgid     = gid.val << 16u,
                wuid     = uid.val;
    return wuid | wgid | wu | wg | wo | reserved;
}

static inline size_t ppcs_n_ppc(ppcs_t ppcs) {
    return FASTR_N_C2W(ppcs.len);
}

static inline ppc_t ppcs_get_ppc(ppcs_t ppcs, size_t i) {
    return ppcs.words[i];
}

static inline size_t ppcs_depth(ppcs_t ppcs) {
    size_t i, n_ppc = ppcs_n_ppc(ppcs), cnt = 0;
    for (i = 0; i < n_ppc; i++) {
        cnt += ppcs.words[i] >> 35u;
    }
    return cnt;
}

static inline void ppc_print(ppc_t w) {
    u8 u, g, o;
    kuid_t uid;
    kgid_t gid;
    size_t depth = ppc_parse(w, &u, &g, &o, &uid, &gid);
    printk(KERN_CONT "%lu(%u%u%u)[uid=%u,gid=%u]", depth,
           u != 0, g != 0, o != 0,
           uid.val, gid.val);
}

static inline void ppcs_print(ppcs_t ppcs, size_t off) {
    size_t i, n_ppc = ppcs_n_ppc(ppcs);

    if (n_ppc) {
        ppc_t first_ppc = ppcs.words[0];
        ppc_set_dep(&first_ppc, ppc_get_dep(first_ppc) - off);
        printk(KERN_CONT "/");
        ppc_print(first_ppc);
    }

    for (i = 1; i < n_ppc; i++) {
        printk(KERN_CONT "/");
        ppc_print(ppcs.words[i]);
    }
}

/* construct PPCs from a PPC array, and number of PPCs in this array */
static inline ppcs_t ppcs_from_narr(ppc_t *ppcarr, size_t n_ppc) {
    return fastr((char *) ppcarr, FASTR_N_W2C(n_ppc));
}

/*
 * construct PPCs from a PPC array, and total dep of PPCs
 * If @exact is set, the specified dep must be equal to a prefix
 * sum of PPCs in PPCarr.
 */
static inline ppcs_t ppcs_from_deparr(ppc_t *ppcarr, size_t dep,
                                      int exact) {
    size_t cur;
    ppc_t *end = ppcarr;
    for (cur = 0; cur < dep; cur += ppc_get_dep(end[-1])) {
        end++;
    }
    BUG_ON(exact && (dep != cur));
    return ppcs_from_narr(ppcarr, end - ppcarr);
}

/* Slightly different from @ppcs_from_deparr: in reversed order */
static inline ppcs_t ppcs_from_deparr_rv(ppc_t *ppcarr_end, size_t dep,
                                         int exact) {
    size_t cur;
    ppc_t *begin = ppcarr_end;
    for (cur = 0; cur < dep; cur += ppc_get_dep(*begin)) {
        begin--;
    }
    BUG_ON(exact && (dep != cur));
    return ppcs_from_narr(begin, ppcarr_end - begin);
}

void ppcs_append(ppcs_t *dst, ppcs_t src, size_t off, size_t depth);
void ppcs_prepend(ppcs_t *dst, ppcs_t src, size_t off, size_t depth);

/*
 * shrink prefix of PPCs, but do it out-of-place (OOP)
 * The OOP means that we do not modify the actual PPC, but return
 * the depth of first PPC instead.
 */
static inline size_t ppcs_shrink_pre_oop(ppcs_t *dst, size_t depth) {
    size_t cur, i, n = ppcs_n_ppc(*dst);
    for (cur = 0, i = 0;
         cur < depth;
         cur += ppc_get_dep(ppcs_get_ppc(*dst, i++)));
    if (unlikely(cur == depth)) {
        /* We do not allow zero-depth PPC, so move on */
        if (i < n) {
            cur += ppc_get_dep(ppcs_get_ppc(*dst, i));
        }
    } else {
        i--;
    }
    dst->words += i;
    dst->len -= i * sizeof(ppc_t);
    return cur - depth;
}

/* just like @ppcs_shrink_pre_oop */
static inline size_t ppcs_shrink_suf_oop(ppcs_t *dst, size_t depth) {
    size_t cur, i, n = ppcs_n_ppc(*dst);
    for (cur = 0, i = n;
         cur < depth;
         cur += ppc_get_dep(ppcs_get_ppc(*dst, --i)));
    if (unlikely(cur == depth)) {
        /* We do not allow zero-depth PPC, so move on */
        if (i > 0) {
            cur += ppc_get_dep(ppcs_get_ppc(*dst, i - 1));
        }
    } else {
        i++;
    }
    dst->len = i * sizeof(ppc_t);
    return cur - depth;
}

/* shrink prefix of PPCs, in-place */
static inline void ppcs_shrink_pre(ppcs_t *dst, size_t depth) {
    size_t depth_fix = ppcs_shrink_pre_oop(dst, depth);
    if (likely(ppcs_n_ppc(*dst))) {
        ppc_set_dep(&dst->words[0], depth_fix);
    }
}

/* shrink suffix of PPCs, in-place */
static inline void ppcs_shrink_suf(ppcs_t *dst, size_t n_level) {
    size_t depth_fix = ppcs_shrink_suf_oop(dst, n_level);
    if (likely(ppcs_n_ppc(*dst))) {
        ppc_set_dep(&dst->words[ppcs_n_ppc(*dst) - 1], depth_fix);
    }
}

static inline void ppcs_append_ppc(ppcs_t *dst, ppc_t ppc) {
    ppcs_t ppcs = ppcs_from_narr(&ppc, ppc_get_dep(ppc));
    ppcs_append(dst, ppcs, 0, ppcs_depth(ppcs));
}

static inline void ppcs_prepend_ppc(ppcs_t *dst, ppc_t ppc) {
    ppcs_t ppcs = ppcs_from_narr(&ppc, ppc_get_dep(ppc));
    ppcs_prepend(dst, ppcs, 0, ppcs_depth(ppcs));
}

static inline int ppcs_is_empty(ppcs_t ppcs) {
    return fastr_is_empty(ppcs);
}

static inline void ppcs_is_clear(ppcs_t *ppcs) {
    fastr_clear(ppcs);
}

/*
 * Compare if @x[x_off:], @y[y_off:] have equal PPCs
 * The @x_off and @y_off are depth offsets. We can use them
 * to skip a few prefix levels of @x and @y.
 */
static inline int ppcs_is_equal(ppcs_t x, size_t x_off,
                                ppcs_t y, size_t y_off) {
    ppc_t x_first, y_first;

    if (ppcs_is_empty(x) || ppcs_is_empty(y)) {
        return ppcs_is_empty(x) && ppcs_is_empty(y);
    }

    x_first = ppcs_get_ppc(x, 0);
    y_first = ppcs_get_ppc(y, 0);
    ppc_set_dep(&x_first, ppc_get_dep(x_first) - x_off);
    ppc_set_dep(&y_first, ppc_get_dep(y_first) - y_off);
    if (x_first != y_first) {
        return 0;
    }

    return !fastr_strcmp(fastr_slice_after(x, sizeof(ppc_t)),
                         fastr_slice_after(y, sizeof(ppc_t)));
}

#endif //_LINUX_PPCS_H
