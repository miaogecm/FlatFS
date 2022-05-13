#ifndef __FXMARK_DEBUG_H__
#define __FXMARK_DEBUG_H__

typedef struct {
    unsigned int slock;
} spinlock_t;

#define SPIN_INIT {0}

static __always_inline spinlock_t *
spin_init(spinlock_t *lock) {
    lock->slock = 0;
    return lock;
}

static __always_inline void
spin_lock(spinlock_t *lock) {
    short inc = 0x0100;
    __asm__ __volatile__ ("  lock; xaddw %w0, %1;"
                          "1:"
                          "  cmpb %h0, %b0;"
                          "  je 2f;"
                          "  rep ; nop;"
                          "  movb %1, %b0;"
                          "  jmp 1b;"
                          "2:"
                          : "+Q" (inc), "+m" (lock->slock)
                          :: "memory", "cc");
}

#define ACCESS_ONCE(x) (* (volatile typeof(x) *) &(x))

static __always_inline void
spin_unlock(spinlock_t *lock) {
    __asm__ __volatile__("lock; incb %0;" : "+m" (lock->slock) :: "memory", "cc");
}

static inline void
cpu_relax(void) {
    __asm__ __volatile__ ("rep; nop;");
}

static inline void
cpu_barrier(void) {
    __asm__ __volatile__ ("mfence" ::: "memory");
}

void fxmark_debug(const char *fmt, ...);
void fxmark_debug_nolock(const char *fmt, ...);

#endif
