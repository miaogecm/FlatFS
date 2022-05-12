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
 *                  /test --> NULL
 */
fastr_t get_parent_path(fastr_t path) {
    size_t pos = fastr_find_last(path, '/');
    char *buffer;
    fastr_t res;
    if (pos == -1 || pos == 0) {
        /* /test or test */
        return FASTR_NULL;
    }
    buffer = kmalloc(pos, GFP_ATOMIC);
    res = fastr(buffer, 0);
    fastr_append(&res, fastr_slice_before(path, pos));
    return res;
}

fastr_t dir_min_key_outer(fastr_t path) {
    char *buffer = kmalloc(path.len + 1, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, '/');
    return fs;
}

fastr_t dir_min_key(fastr_t path) {
    char *buffer = kmalloc(path.len + 2, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, '/');
    fastr_append_ch(&fs, ASCII_FIRST);
    return fs;
}

fastr_t dir_max_key_outer(fastr_t path) {
    char *buffer = kmalloc(path.len + 2, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, '/');
    fastr_append_ch(&fs, (char) (ASCII_LAST + 1));
    return fs;
}

fastr_t dir_max_key(fastr_t path) {
    char *buffer = kmalloc(path.len + 2, GFP_ATOMIC);
    fastr_t fs = fastr(buffer, 0);
    fastr_append(&fs, path);
    fastr_append_ch(&fs, '/');
    fastr_append_ch(&fs, ASCII_LAST);
    return fs;
}
