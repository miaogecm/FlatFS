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

#ifndef _LINUX_PATHMAN_H
#define _LINUX_PATHMAN_H

#include <linux/fastr.h>

fastr_t get_parent_path(fastr_t path);

fastr_t dir_min_key_outer(fastr_t path);
fastr_t dir_min_key(fastr_t path);
fastr_t dir_max_key_outer(fastr_t path);
fastr_t dir_max_key(fastr_t path);

static inline size_t get_path_depth(fastr_t path) {
    size_t i, cnt = 0;
    for (i = 0; i < path.len; i++) {
        if (path.chars[i] == '/') {
            cnt++;
        }
    }
    return cnt;
}

#endif //_LINUX_PATHMAN_H
