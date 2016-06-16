/*
 * sched.h
 */

#ifndef _SCHED_H
#define _SCHED_H

#include <klibc/extern.h>
#include <features.h>

#include <stddef.h>
#include <sys/types.h>
#include <bits/sched.h>
/* Define the real names for the elements of `struct sched_param'.  */
#define sched_priority	__sched_priority

#if 0
/* linux/sched.h is unusable; put the declarations we need here... */
#define SCHED_OTHER             0
#define SCHED_FIFO              1
#define SCHED_RR                2

struct sched_param {
	int sched_priority;
};
#endif

__extern int sched_setscheduler(pid_t, int, const struct sched_param *);
__extern int sched_getscheduler(pid_t);
__extern int sched_setaffinity(pid_t, unsigned int, unsigned long *);
__extern int sched_getaffinity(pid_t, unsigned int, unsigned long *);
__extern int sched_yield(void);
__extern int sched_get_priority_min(int);
__extern int sched_get_priority_max(int);
__extern int sched_getparam (pid_t, struct sched_param *);
__extern int sched_setscheduler (pid_t, int , const struct sched_param *);


/* Raw interfaces to clone(2); only actually usable for non-VM-cloning */
#ifdef __ia64__
__extern pid_t __clone2(int, void *, void *);
static __inline__ pid_t __clone(int _f, void *_sp)
{
	/* If this is used with _sp != 0 it will have the effect of the sp
	   and rsp growing away from a single point in opposite directions. */
	return __clone2(_f, _sp, _sp);
}
#else
__extern pid_t __clone(int, void *);
#endif

#endif				/* _SCHED_H */
