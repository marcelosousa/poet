/*
 * isatty.c
 */

#include <unistd.h>
#include <termios.h>
#include <errno.h>

#include <klibc/poet.h>

int isatty(int fd)
{
#ifdef KLIBC_ISATTY_EASY
   return fd >= 0 && fd <= 2;
#else
	struct termios dummy;
	/* All ttys support TIOCGPGRP */
	/* except /dev/console which needs TCGETS */
	return !ioctl(fd, TCGETS, &dummy);
#endif
}
