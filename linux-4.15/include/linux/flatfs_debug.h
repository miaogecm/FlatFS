/*
 * FlatFS: A Metadata-Optimized NVM File System
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */
#ifndef FLATFS_DEBUG_H
#define FLATFS_DEBUG_H

#include <linux/flatfs_define.h>

#define FLATFS_DEBUG(subsystem)           CONFIG_FLATFS_DEBUG_##subsystem

#define FLATFS_INFO(subsystem, fmt, ...)  do {                                                  \
    if (FLATFS_DEBUG(subsystem)) {                                                              \
        printk("%s[%d]: (" #subsystem " syscall=%s/%d) " fmt "\n",                              \
               __func__, __current_pid(),                                                       \
               __current_syscall_name(), __current_syscall_nr(),                                \
               ##__VA_ARGS__);                                                                  \
    }                                                                                           \
} while (0)

int __current_syscall_nr(void);
const char *__current_syscall_name(void);
int __current_pid(void);

#endif
