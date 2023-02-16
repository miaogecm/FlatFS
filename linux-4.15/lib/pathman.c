/*
 * FlatFS: A Metadata-Optimized NVM File System
 * 
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 * Helper functions for string file path
 */

#include <linux/flatfs.h>
#include <linux/pathman.h>
#include <linux/slab.h>

/*
 * get_parent_path: /a/b/test --> /a/b
 *                  /test, test --> NULL
 */
fastr_t get_parent_path(fastr_t path) {
    size_t pos = fastr_find_last(path, CTLCHR_COMPONENT_SEPARATOR);
    char *buffer;
    fastr_t res;
    if (pos == FASTR_NPOS || pos == 0) {
        return FASTR_NULL;
    }
    buffer = kmalloc(pos, GFP_ATOMIC);
    res = fastr(buffer, 0);
    fastr_append(&res, fastr_slice_before(path, pos));
    return res;
}

fastr_t dir_path_upperbound(fastr_t path) {
    char *buffer = kmalloc(path.len + 2, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, CTLCHR_COMPONENT_SEPARATOR);
    fastr_append_ch(&fs, '\x7f');
    return fs;
}

fastr_t dir_path_lowerbound(fastr_t path) {
    char *buffer = kmalloc(path.len + 1, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, CTLCHR_COMPONENT_SEPARATOR);
    return fs;
}

size_t get_path_depth(fastr_t path) {
    size_t i, cnt = 0;
    for (i = 0; i < path.len; i++) {
        if (path.chars[i] == CTLCHR_COMPONENT_SEPARATOR) {
            cnt++;
        }
    }
    return cnt;
}
