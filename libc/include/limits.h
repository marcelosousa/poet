/*
 * limits.h
 */

#ifndef _LIMITS_H
#define _LIMITS_H

/* No multibyte characters seen */
#define MB_LEN_MAX 1

#define OPEN_MAX        256

#define CHAR_BIT	8
#define SHRT_BIT	16
#define INT_BIT		32
#define LONGLONG_BIT	64

#define SCHAR_MIN	(-128)
#define SCHAR_MAX	127
#define UCHAR_MAX	255

#ifdef __CHAR_UNSIGNED__
# define CHAR_MIN 0
# define CHAR_MAX UCHAR_MAX
#else
# define CHAR_MIN SCHAR_MIN
# define CHAR_MAX SCHAR_MAX
#endif

#define SHRT_MIN	(-32768)
#define SHRT_MAX	32767
#define USHRT_MAX	65535

#define INT_MIN		(-2147483647-1)
#define INT_MAX		2147483647
#define UINT_MAX	4294967295U

#define LLONG_MIN	(-9223372036854775807LL-1)
#define LLONG_MAX	9223372036854775807LL
#define ULLONG_MAX	18446744073709551615ULL

#include <bitsize/limits.h>



/*
 * #include <linux/limits.h>
 * I copy paste the version in my computer
 * -- Cesar
 */


// BEGIN /usr/include/linux/limits.h

#define NR_OPEN	        1024

#define NGROUPS_MAX    65536	/* supplemental group IDs are available */
#define ARG_MAX       131072	/* # bytes of args + environ for exec() */
#define LINK_MAX         127	/* # links a file may have */
#define MAX_CANON        255	/* size of the canonical input queue */
#define MAX_INPUT        255	/* size of the type-ahead buffer */
#define NAME_MAX         255	/* # chars in a file name */
#define PATH_MAX        4096	/* # chars in a path name including nul */
#define PIPE_BUF        4096	/* # bytes in atomic write to a pipe */
#define XATTR_NAME_MAX   255	/* # chars in an extended attribute name */
#define XATTR_SIZE_MAX 65536	/* size of an extended attribute value (64k) */
#define XATTR_LIST_MAX 65536	/* size of extended attribute namelist (64k) */

#define RTSIG_MAX	  32

// END /usr/include/linux/limits.h

#define SSIZE_MAX	LONG_MAX

#endif				/* _LIMITS_H */
