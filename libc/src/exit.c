/*
 * exit.c
 *
 * exit(), including the handling of the atexit chain.
 */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/syscall.h>
#include "atexit.h"

#include <klibc/poet.h>

/* Link chain for atexit/on_exit */
struct atexit *__atexit_list;

__noreturn exit(int rv)
{
#ifdef KLIBC_ATEXIT_CALLBACKS
	struct atexit *ap;

	for (ap = __atexit_list; ap; ap = ap->next) {
		/* This assumes extra args are harmless.  They should
		   be in all normal C ABIs, but if an architecture has
		   some particularly bizarre ABI this might be worth
		   watching out for. */
		ap->fctn(rv, ap->arg);
	}
#endif
	/* Handle any library destructors if we ever start using them... */
	fflush(NULL);

	_exit(rv);
}
