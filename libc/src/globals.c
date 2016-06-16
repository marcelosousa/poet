/*
 * globals.c
 *
 * These have to be defined somewhere...
 */
#include <errno.h>
#include <unistd.h>

int __errno;
char **environ;

__extern int *__errno_location ()
{
   return &__errno;
}
