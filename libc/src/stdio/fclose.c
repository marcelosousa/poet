/*
 * fclose.c
 */

#include "stdioint.h"

int fclose_simple(FILE *file)
{
   return close (file->_IO_fileno);
}

int fclose_orig(FILE *file)
{
	struct _IO_file_pvt *f = stdio_pvt(file);
	int rv;

	fflush(file);

	rv = close(f->pub._IO_fileno);

	/* Remove from linked list */
	f->next->prev = f->prev;
	f->prev->next = f->next;

	free(f);
	return rv;
}

int fclose(FILE *file)
{
#ifdef KLIBC_STREAMS_ORIG
   return fclose_orig (file);
#else
   return fclose_simple (file);
#endif
}
