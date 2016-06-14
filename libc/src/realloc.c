/*
 * realloc.c
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <klibc/poet.h>

#include "malloc.h"


/* FIXME: This is cheesy, it should be fixed later */

void *realloc_impl_orig(void *ptr, size_t size)
{
	struct free_arena_header *ah;
	void *newptr;
	size_t oldsize;

	if (!ptr)
		return malloc(size);

	if (size == 0) {
		free(ptr);
		return NULL;
	}

	/* Add the obligatory arena header, and round up */
	size = (size + 2 * sizeof(struct arena_header) - 1) & ARENA_SIZE_MASK;

	ah = (struct free_arena_header *)
	    ((struct arena_header *)ptr - 1);

	if (ah->a.size >= size && size >= (ah->a.size >> 2)) {
		/* This field is a good size already. */
		return ptr;
	} else {
		/* Make me a new block.  This is kind of bogus; we should
		   be checking the following block to see if we can do an
		   in-place adjustment... fix that later. */

		oldsize = ah->a.size - sizeof(struct arena_header);

		newptr = malloc(size);
		memcpy(newptr, ptr, (size < oldsize) ? size : oldsize);
		free(ptr);

		return newptr;
	}
}

void *realloc_impl_static(void *ptr, size_t size)
{
   void *ptr2;
	if (!ptr) return malloc(size);
   ptr2 = malloc (size);
   if (ptr2) memcpy (ptr2, ptr, size);
   free (ptr);
   return ptr2;
}

void *realloc(void *ptr, size_t size)
{
#ifdef KLIBC_MALLOC_DEBUG
   printf ("klibc: realloc: ptr %-16p size %zu\n", ptr, size);
#endif

#ifdef KLIBC_MALLOC_ORIG
   return realloc_impl_orig (ptr, size);
#else
#ifdef KLIBC_MALLOC_STATIC
   return realloc_impl_static (ptr, size);
#else
#error Please, select exactly one of KLIBC_MALLOC_{ORIG,STATIC}
#endif // STATIC
#endif // ORIG
}
