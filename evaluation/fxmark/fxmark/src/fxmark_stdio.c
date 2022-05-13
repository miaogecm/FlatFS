#include <stdarg.h>
#include <sys/types.h>

#define NULL ((void*)0)

static unsigned long long
__fxmark_getuint(va_list ap, int lflag) {
    if (lflag >= 2) {
        return va_arg(ap, unsigned long long);
    }
    else if (lflag) {
        return va_arg(ap, unsigned long);
    }
    else {
        return va_arg(ap, unsigned int);
    }
}

static long long
__fxmark_getint(va_list ap, int lflag) {
    if (lflag >= 2) {
        return va_arg(ap, long long);
    }
    else if (lflag) {
        return va_arg(ap, long);
    }
    else {
        return va_arg(ap, int);
    }
}

size_t
__fxmark_strnlen(const char *s, size_t len) {
    size_t cnt = 0;
    while (cnt < len && *s ++ != '\0') {
        cnt ++;
    }
    return cnt;
}

static void
__fxmark_printnum(void (*putch)(int, void *), void *putdat,
        unsigned long long num, unsigned base, int width, int padc) {
    unsigned long long result = num / base;
    unsigned mod = num % base;
    if (num >= base) {
        __fxmark_printnum(putch, putdat, result, base, width - 1, padc);
    } else {
        while (-- width > 0)
            putch(padc, putdat);
    }
    putch("0123456789abcdef"[mod], putdat);
}

static void
__fxmark_vprintfmt(void (*putch)(int, void *), void *putdat, const char *fmt, va_list ap) {
    register const char *p;
    register int c;
    unsigned long long num;
    int base, width, precision, lflag, altflag;

    while (1) {
        while ((c = *(unsigned char *)fmt ++) != '%') {
            if (c == '\0') {
                return;
            }
            putch(c, putdat);
        }
        char padc = ' ';
        width = precision = -1;
        lflag = altflag = 0;
    reswitch:
        switch (c = *(unsigned char *)fmt ++) {
        case '-':
            padc = '-';
            goto reswitch;
        case '0':
            padc = '0';
            goto reswitch;
        case '1' ... '9':
            for (precision = 0; ; ++ fmt) {
                precision = precision * 10 + c - '0';
                c = *fmt;
                if (c < '0' || c > '9') {
                    break;
                }
            }
            goto process_precision;
        case '*':
            precision = va_arg(ap, int);
            goto process_precision;
        case '.':
            if (width < 0)
                width = 0;
            goto reswitch;
        case '#':
            altflag = 1;
            goto reswitch;
        process_precision:
            if (width < 0)
                width = precision, precision = -1;
            goto reswitch;
        case 'l':
            lflag ++;
            goto reswitch;
        case 'c':
            putch(va_arg(ap, int), putdat);
            break;
        case 's':
            if ((p = va_arg(ap, char *)) == NULL) {
                p = "(null)";
            }
            if (width > 0 && padc != '-') {
                for (width -= __fxmark_strnlen(p, precision); width > 0; width --) {
                    putch(padc, putdat);
                }
            }
            for (; (c = *p ++) != '\0' && (precision < 0 || -- precision >= 0); width --) {
                if (altflag && (c < ' ' || c > '~')) {
                    putch('?', putdat);
                }
                else {
                    putch(c, putdat);
                }
            }
            for (; width > 0; width --) {
                putch(' ', putdat);
            }
            break;
        case 'd':
            num = __fxmark_getint(ap, lflag);
            if ((long long)num < 0) {
                putch('-', putdat);
                num = -(long long)num;
            }
            base = 10;
            goto number;
        case 'u':
            num = __fxmark_getuint(ap, lflag);
            base = 10;
            goto number;
        case 'o':
            num = __fxmark_getuint(ap, lflag);
            base = 8;
            goto number;
        case 'p':
            putch('0', putdat);
            putch('x', putdat);
            num = (unsigned long long)va_arg(ap, void *);
            base = 16;
            goto number;
        case 'x':
            num = __fxmark_getuint(ap, lflag);
            base = 16;
        number:
            __fxmark_printnum(putch, putdat, num, base, width, padc);
            break;
        case '%':
            putch(c, putdat);
            break;
        default:
            putch('%', putdat);
            for (fmt --; fmt[-1] != '%'; fmt --)
                /* do nothing */;
            break;
        }
    }
}

struct __fxmark_sprintbuf {
    char *begin, *end;
    int cnt;
};

static void
__fxmark_sprintputch(int c, struct __fxmark_sprintbuf *b) {
    b->cnt ++;
    if (b->begin < b->end) {
        *(b->begin ++) = c;
    }
}

int
__fxmark_vsnprintf(char *s_buf, size_t s_size, const char *fmt, va_list ap) {
    struct __fxmark_sprintbuf b = {s_buf, s_buf + s_size - 1, 0};
    if (s_buf == NULL || b.begin > b.end) {
        return -1;
    }
    __fxmark_vprintfmt((void (*)(int, void *))__fxmark_sprintputch, &b, fmt, ap);
    *(b.begin) = '\0';
    return b.cnt;
}
