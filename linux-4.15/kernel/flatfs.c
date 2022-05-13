/*
 * FlatFS: A Metadata-Optimized NVM File System
 *
 * Hohai University
 *
 * Miao Cai, mcai@hhu.edu.cn
 * Junru Shen, gnu_emacs@hhu.edu.cn
 *
 */

#include <linux/syscalls.h>
#include <linux/kernel.h>
#include <linux/trace.h>
#include <asm/syscall.h>
#include <linux/flatfs_define.h>

#ifdef CONFIG_FTRACE_SYSCALLS
const char *get_syscall_name(int syscall);
#else
static inline const char *get_syscall_name(int syscall) {
	return NULL;
}
#endif

int __current_syscall_nr(void) {
    return syscall_get_nr(current, task_pt_regs(current));
}

const char *__current_syscall_name(void) {
    int nr = __current_syscall_nr();
    return get_syscall_name(nr) ? : "?";
}

int __current_pid(void) {
    return current->pid;
}
