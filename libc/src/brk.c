/* brk.c - Change data segment size */

/* Written 2000 by Werner Almesberger */

#include <stddef.h>
#include <unistd.h>
#include <sys/types.h>
#include "malloc.h"
#include "unimpl.h"

// Cesar: brk and sbrk are usually libc functions, but quite standard so we
// don't implement them
#if 0
#if !_KLIBC_NO_MMU		/* uClinux doesn't have brk() */

char *__current_brk;

/*
 * The Linux brk() isn't what most people expect, so we call the
 * system call __brk() and provide a wrapper.
 */
int brk(void *end_data_segment)
{
	char *new_brk;

   UNIMPL ();

	new_brk = __brk(end_data_segment);
	//new_brk = 0;
	if (new_brk != end_data_segment)
		return -1;
	__current_brk = new_brk;
	return 0;
}

#endif

#endif
