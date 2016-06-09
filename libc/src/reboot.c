/*
 * reboot.c
 */

#include <unistd.h>
#include <sys/reboot.h>
#include <sys/syscall.h>

// Cesar: reboot is a system call (glibc has it, but I'm not sure about the
// number of arguments)
#if 0
/* This provides the one-argument glibc-ish version of reboot.
   The full four-argument system call is available as __reboot(). */

int reboot(int flag)
{
	return __reboot(LINUX_REBOOT_MAGIC1, LINUX_REBOOT_MAGIC2, flag, NULL);
}
#endif
