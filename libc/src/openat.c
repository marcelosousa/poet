/*
 * openat.c
 *
 * On 32-bit platforms we need to pass O_LARGEFILE to the openat()
 * system call, to indicate that we're 64-bit safe.
 */

#define _KLIBC_IN_OPEN_C
#include <unistd.h>
#include <fcntl.h>
#include <bitsize.h>

// Cesar
int openat3 (int dirfd, const char *pathname, int flags)
{
	return openat(dirfd, pathname, flags | O_LARGEFILE, 0);
}

#if 0
extern int __openat(int, const char *, int, mode_t);

#if _BITSIZE == 32 && !defined(__i386__) && !defined(__m68k__) && defined(__NR_openat)

int openat(int dirfd, const char *pathname, int flags, mode_t mode)
{
	return __openat(dirfd, pathname, flags | O_LARGEFILE, mode);
}

#endif
#endif
