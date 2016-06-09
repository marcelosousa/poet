/*
 * ppoll.c
 */

#include <sys/poll.h>
#include <sys/syscall.h>
#include "unimpl.h"

#ifdef __NR_ppoll

__extern int __ppoll(struct pollfd *, nfds_t, struct timespec *,
		     const sigset_t *, size_t);

int ppoll(struct pollfd *ufds, nfds_t nfds, struct timespec *timeout,
	  const sigset_t * sigmask)
{
   UNIMPL ();
	//return __ppoll(ufds, nfds, timeout, sigmask, sizeof *sigmask);
	return -1;
}

#endif
