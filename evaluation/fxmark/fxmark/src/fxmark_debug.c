#include <stdarg.h>
#include <sys/types.h>
#include "fxmark_debug.h"

#define __NR_mofs_print	334

static spinlock_t stdio_lock = SPIN_INIT;

int __fxmark_vsnprintf(char *s_buf, size_t s_size, const char *fmt, va_list ap);

static void
__fxmark_debug_sub(int need_lock, const char *buf, size_t len) {
    if (need_lock) {
        fxmark_debug_lock();
    }

    register long arg0 __asm__ ("rdi") = (long)buf;
    register long arg1 __asm__ ("rsi") = (long)len;

	/* just use the printk function in kernel */
    __asm__ __volatile__ ("syscall"
                          :: "a" (__NR_mofs_print), "r" (arg0), "r" (arg1)
                          : "memory", "cc", "r11", "cx");

    if (need_lock) {
        fxmark_debug_unlock();
    }
}

void fxmark_debug(const char *fmt, ...) {
    char s_buf[4096];
    va_list arg;
    int done;
    va_start(arg, fmt);
    done = __fxmark_vsnprintf(s_buf, sizeof(s_buf), fmt, arg);
    va_end(arg);
    if (done > 0) {
        __fxmark_debug_sub(1, s_buf, done);
    }
}

void fxmark_debug_nolock(const char *fmt, ...) {
    char s_buf[4096];
    va_list arg;
    int done;
    va_start(arg, fmt);
    done = __fxmark_vsnprintf(s_buf, sizeof(s_buf), fmt, arg);
    va_end(arg);
    if (done > 0) {
        __fxmark_debug_sub(0, s_buf, done);
    }
}

void fxmark_debug_lock(void) {
    spin_lock(&stdio_lock);
}

void fxmark_debug_unlock(void) {
    spin_unlock(&stdio_lock);
}
