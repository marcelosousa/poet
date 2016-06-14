/*
 * fflush.c
 */

#include "stdioint.h"

#include <klibc/poet.h>

int __fflush(struct _IO_file_pvt *f)
{
	ssize_t rv;
	char *p;

// Cesar: this creates an infinite loop if you fflush(f) and f->ibytes != 0 and
// f->obytes != 0 !!!
#if 0
	/*
	 * Flush any unused input data.  If there is input data, there
	 * won't be any output data.
	 */
	if (__unlikely(f->ibytes))
		return fseek(&f->pub, 0, SEEK_CUR);
#endif

	p = f->buf;
	while (f->obytes) {
		rv = write(f->pub._IO_fileno, p, f->obytes);
		if (rv == -1) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			f->pub._IO_error = true;
			return EOF;
		} else if (rv == 0) {
			/* EOF on output? */
			f->pub._IO_eof = true;
			return EOF;
		}

		p += rv;
		f->obytes -= rv;
	}

	return 0;
}

int fflush(FILE *file)
{
#ifdef KLIBC_STREAMS_ORIG
	struct _IO_file_pvt *f;

	if (__likely(file)) {
		f = stdio_pvt(file);
		return __fflush(f);
	} else {
		int err = 0;

		for (f = __stdio_headnode.next;
		     f != &__stdio_headnode;
		     f = f->next) {
			if (f->obytes)
				err |= __fflush(f);
		}
		return err;
	}
#else
   return 0;
#endif
}


