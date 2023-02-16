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

fastr_t dir_path_upperbound(fastr_t path);
fastr_t dir_path_lowerbound(fastr_t path);

fastr_t get_parent_path(fastr_t path);

size_t  get_path_depth(fastr_t path);

#endif //_LINUX_PATHMAN_H
