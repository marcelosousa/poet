#include "stdioint.h"

off_t ftell(FILE *file)
{
#ifdef KLIBC_STREAMS_ORIG
	struct _IO_file_pvt *f = stdio_pvt(file);
	off_t pos = lseek(f->pub._IO_fileno, 0, SEEK_CUR);

	if (pos >= 0)
		pos += (int)f->obytes - (int)f->ibytes;

	return pos;
#else
	return lseek(file->_IO_fileno, 0, SEEK_CUR);
#endif
}
