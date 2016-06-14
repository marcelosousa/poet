/*
 * ungetc.c
 */

#include "stdioint.h"
#include <assert.h>

int ungetc_simple(int c, FILE *file)
{
   assert (0 && "ungetc unimplemented in our simple libc streams library");
   return EOF;
}

int ungetc_orig(int c, FILE *file)
{
	struct _IO_file_pvt *f = stdio_pvt(file);

	if (f->obytes || f->data <= f->buf)
		return EOF;

	*(--f->data) = c;
	f->ibytes++;
	return c;
}

int ungetc(int c, FILE *file)
{
#ifdef KLIBC_STREAMS_ORIG
   return ungetc_orig (c, file);
#else
   return ungetc_simple (c, file);
#endif
}

