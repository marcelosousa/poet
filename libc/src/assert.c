/*
 * assert.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <klibc/compiler.h>

__noreturn __assert_fail(const char *expr, const char *file, unsigned int line, const char *func)
{
	//printf("Assertion %s failed, file %s, line %u\n", expr, file, line);
	fputs(file, stdout);
	fputs(":", stdout);
	fputs("??: ", stdout);
	fputs(func, stdout);
	fputs(": Assertion failed: ", stdout);
	fputs(expr, stdout);
	fputs("\n", stdout);
	abort();
}
