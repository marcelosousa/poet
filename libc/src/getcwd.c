/*
 * getcwd.c
 *
 * The system call behaves differently than the library function.
 */

#include <unistd.h>
#include <sys/syscall.h>

// Cesar: getcwd is a system call (glibc has it)
#if 0
extern int __getcwd(char *buf, size_t size);

char *getcwd(char *buf, size_t size)
{
	return (__getcwd(buf, size) < 0) ? NULL : buf;
}
#endif
