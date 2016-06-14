/*
 * perror.c
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>

void perror(const char *s)
{
	int e = errno;

	//fprintf(stderr, "%s: %s\n", s, strerror(e));
   fputs (s, stderr);
   fputs (": ", stderr);
   fputs (strerror(e), stderr);
   fputs ("\n", stderr);
}
