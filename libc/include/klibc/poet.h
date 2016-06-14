
#ifndef _KLIBC_POET_H
#define _KLIBC_POET_H

#include <stdio.h>
#include <inttypes.h>

/* select an implementation for malloc/realloc/free, see at src/malloc.c */
#undef KLIBC_MALLOC_ORIG
#define KLIBC_MALLOC_STATIC

/* if you select KLIBC_MALLOC_STATIC, then how big the static buffer should be
 */
#define KLIBC_MALLOC_STATIC_SIZE (64 * 1024 * 1024)

/* print information about the malloc/realloc/free calls, useful for
 * understanding how a real program behaves with regard to memory allocation
 */
#undef KLIBC_MALLOC_DEBUG

/* isatty is called on fopen(3) to determine the buffering strategy for the
 * file; when this is set isatty will return true for 0,1,2 and false otherwise,
 * instead of doing an ioctl
 */
#define KLIBC_ISATTY_EASY

/* exit(3) will execute the callbacks registered via atexit(3) iff this is
 * defined
 */
#undef KLIBC_ATEXIT_CALLBACKS

/*
 */
#undef KLIBC_STREAMS_ORIG


/* since we won't have printf, it's nice to have a way to print a number, if we
 * need so; implemented in src/printf.c
 */
void format_int2 (char *q, size_t n, uintmax_t val, int base, int sgn);

#endif

