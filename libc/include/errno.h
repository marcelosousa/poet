/*
 * errno.h
 */

#ifndef _ERRNO_H
#define _ERRNO_H

#include <klibc/extern.h>
#include <asm/errno.h>

__extern int *__errno_location ();

#ifndef errno
# define errno (*__errno_location ())
#endif

#endif /* _ERRNO_H */
