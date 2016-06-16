/*
 * time.h
 */

#ifndef _TIME_H
#define _TIME_H

#include <klibc/extern.h>
#include <bits/types.h>
#include <sys/time.h>

typedef __clock_t clock_t;
typedef __clockid_t clockid_t;

__extern clock_t clock (void);
__extern time_t time(time_t *);
__extern int nanosleep(const struct timespec *, struct timespec *);

/* klibc-specific but useful since we don't have floating point */
__extern char *strtotimeval(const char *str, struct timeval *tv);
__extern char *strtotimespec(const char *str, struct timespec *tv);

__extern int clock_getres(clockid_t clk_id, struct timespec *res);
__extern int clock_gettime(clockid_t clk_id, struct timespec *tp);
__extern int clock_settime(clockid_t clk_id, const struct timespec *tp);


#endif				/* _TIME_H */
