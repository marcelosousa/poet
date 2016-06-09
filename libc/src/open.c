/*
 * open.c
 *
 * On 32-bit platforms we need to pass O_LARGEFILE to the open()
 * system call, to indicate that we're 64-bit safe.
 *
 * For 64 bit systems without the open syscall, pass straight
 * through into openat.
 */

//#define _KLIBC_IN_OPEN_C
#include <unistd.h>
#include <fcntl.h>
#include <bitsize.h>
#include <sys/syscall.h>

// Cesar
int open2(const char *pathname, int flags)
{
   return open (pathname, flags, 0); // open is undefined, the linker will find it
}


#if 0
#ifndef __NR_open
#if _BITSIZE == 32

// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

extern int __openat(int, const char *, int, mode_t);

int open3(const char *pathname, int flags, mode_t mode)
{
	return __openat(AT_FDCWD, pathname, flags | O_LARGEFILE, mode);
}

#else

__extern int openat4(int, const char *, int, mode_t);

int open3(const char *pathname, int flags, mode_t mode)
{
	return openat4 (AT_FDCWD, pathname, flags, mode);
}

#endif /* _BITSIZE == 32 */

// Cesar
__extern int open3(const char *, int, mode_t mode);
int open(const char *pathname, int flags)
{
   return open3 (pathname, flags, 0);
}

// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#elif _BITSIZE == 32 && !defined(__i386__) && !defined(__m68k__)

extern int __open(const char *, int, mode_t);

int open3(const char *pathname, int flags, mode_t mode)
{
	return __open(pathname, flags | O_LARGEFILE, mode);
}

// Cesar
__extern int open3(const char *, int, mode_t mode);
int open(const char *pathname, int flags)
{
   return open3 (pathname, flags, 0);
}

#endif /* __NR_open */


#endif
