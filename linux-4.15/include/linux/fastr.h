/*
 * FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Fast string operations
 */

#ifndef _LINUX_FASTR_H
#define _LINUX_FASTR_H

#include <linux/types.h>
#include <linux/string.h>
#include <linux/bug.h>

#include <asm/word-at-a-time.h>

typedef struct {
    union {
        unsigned char *chars;
        unsigned long *words;
    };
    size_t len;
} fastr_t;

#define FASTR_N_W2C(nw)       ((nw) * sizeof(unsigned long))
#define FASTR_N_C2W(nc)       ((nc) / sizeof(unsigned long))

#define FASTR_FMT(x)          (int) (x).len, (x).chars

#define FASTR_LITERAL(s)      fastr((s), sizeof(s) - 1)
#define FASTR_NULL            ((fastr_t) { 0 })
#define FASTR_IS_NOT_NULL(s)  ((s).chars != NULL)

#define FASTR_NPOS            (-1)

static inline fastr_t fastr(char *str, size_t len) {
    return (fastr_t) { { (unsigned char *) str }, len };
}

static inline fastr_t fastr_slice(fastr_t fs, off_t off, size_t len) {
    fs.chars += off;
    fs.len = len;
    return fs;
}

static inline fastr_t fastr_slice_after(fastr_t fs, off_t off) {
    fs.chars += off;
    fs.len -= off;
    return fs;
}

static inline fastr_t fastr_slice_lastn(fastr_t fs, size_t len) {
    fs.chars += fs.len - len;
    fs.len = len;
    return fs;
}

static inline fastr_t fastr_slice_before(fastr_t fs, size_t len) {
    fs.len = len;
    return fs;
}

static inline unsigned long fastr_first_zpword(fastr_t fs) {
    unsigned long word;
    if (!fs.len) {
        return 0;
    }
    word = load_unaligned_zeropad(fs.chars);
    if (fs.len < sizeof(unsigned long)) {
        word &= ((1ul << (fs.len * BITS_PER_BYTE)) - 1);
    }
    return word;
}

static inline fastr_t fastr_eliminate_word(fastr_t *fs1, fastr_t *fs2) {
    unsigned long *str1 = fs1->words, *str2 = fs2->words;
    size_t total_len = min(fs1->len, fs2->len);
    size_t cnt = total_len / sizeof(unsigned long);
    size_t n = (cnt + 7) / 8, len = 0;

    if (!cnt) {
        goto out;
    }

    /* the Duff's Device loop-unrolling technique */
#define FSCPW_DO    if (*str1 != *str2) goto out; \
                    str1++; str2++; len += 8
    switch (cnt % 8) {
        case 0: do {    FSCPW_DO;
                case 7:         FSCPW_DO;
                case 6:         FSCPW_DO;
                case 5:         FSCPW_DO;
                case 4:         FSCPW_DO;
                case 3:         FSCPW_DO;
                case 2:         FSCPW_DO;
                case 1:         FSCPW_DO;
            } while (--n > 0);
    }

out:
    fs1->words = str1;
    fs1->len -= len;
    fs2->words = str2;
    fs2->len -= len;

    return (fastr_t) { { fs1->chars - len }, len };
}

static inline fastr_t fastr_eliminate(fastr_t *fs1, fastr_t *fs2) {
    fastr_t pre = fastr_eliminate_word(fs1, fs2);
    unsigned long p = fastr_first_zpword(*fs1);
    unsigned long q = fastr_first_zpword(*fs2);
    size_t len;

    if (p == q) {
        BUG_ON(fs1->len != fs2->len);
        len = fs1->len;
    } else {
        len = __ffs64(p ^ q) >> 3;
    }

    fs1->chars += len;
    fs1->len -= len;
    fs2->chars += len;
    fs2->len -= len;

    return (fastr_t) { { pre.chars }, len + pre.len };
}

static inline int fastr_wordcmp(unsigned long p, unsigned long q) {
    if (p == q) {
        return 0;
    }

    p = swab64(p);
    q = swab64(q);
    if (p < q) {
        return -1;
    } else if (p > q) {
        return 1;
    }

    /* CF can't reach here, just to make the compiler happy */
    return 0;
}

static inline int fastr_strcmp(fastr_t fs1, fastr_t fs2) {
    unsigned long p, q;
    fastr_eliminate_word(&fs1, &fs2);
    p = fastr_first_zpword(fs1);
    q = fastr_first_zpword(fs2);
    return fastr_wordcmp(p, q);
}

static inline size_t fastr_sizeof(fastr_t fs) {
    return fs.len;
}

static inline void fastr_append(fastr_t *dst, fastr_t src) {
    memcpy(dst->chars + dst->len, src.chars, src.len);
    dst->len += src.len;
}

static inline void fastr_append_ch(fastr_t *dst, char ch) {
    dst->chars[dst->len++] = ch;
}

static inline void fastr_prepend(fastr_t *dst, fastr_t src) {
    memcpy(dst->chars -= src.len, src.chars, src.len);
    dst->len += src.len;
}

static inline void fastr_shrink_pre(fastr_t *dst, size_t n) {
    dst->chars += n;
    dst->len -= n;
}

static inline void fastr_reserve_pre(fastr_t *dst, size_t n) {
    dst->len = n;
}

static inline void fastr_reserve_suf(fastr_t *dst, size_t n) {
    dst->chars += dst->len - n;
    dst->len = n;
}

static inline void fastr_shrink_suf(fastr_t *dst, size_t n) {
    dst->len -= n;
}

static inline int fastr_is_empty(fastr_t str) {
    return !str.len;
}

static inline void fastr_clear(fastr_t *str) {
    str->len = 0;
}

static inline size_t fastr_find_last(fastr_t haystack, char needle) {
    size_t pos;
    for (pos = haystack.len - 1;
         pos != FASTR_NPOS && haystack.chars[pos] != needle;
         pos--);
    return pos;
}

static inline int fastr_is_prefix(fastr_t str, fastr_t pre) {
    fastr_eliminate(&str, &pre);
    return fastr_is_empty(pre);
}

static inline size_t fastr_prefix_len(fastr_t s1, fastr_t s2) {
    return fastr_eliminate(&s1, &s2).len;
}

#endif //_LINUX_FASTR_H
