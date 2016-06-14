
#include "printf2.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#include <signal.h>


void test1 (int argc, char **argv)
{
   const char *s = "hello world\n";
   int len = strlen (s);
   int ret;

   puts ("test1: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

   ret = write (1, s, len);
   fwrite (s, len, 1, stdout);
   if (ret != len)
      puts ("len != ret");
   else
      puts ("len == ret !!");
}

void test2 (int argc, char **argv)
{
   puts ("test2: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   fputs ("test222222222222222222\n", stdout);
   printf2 ("argc %d; argv %p\n", argc, argv);
}

void test3 (int argc, char **argv)
{
   int fd;
   char buff[64];
   size_t ret;

   puts ("test3: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   fd = open2 ("./main.c", O_RDONLY);
   printf2 ("test3: open: fd %d\n", fd);
   ret = read (fd, buff, 64);
   printf2 ("test3: read: ret %zd\n", ret);
   printf2 ("test3: read: buff '%.*s'\n", (int) ret, buff);
   ret = close (fd);
   printf2 ("test3: close: ret %zd\n", ret);
   putss ("test3: ret: ");
   putlu (ret);
}

void test4 (int argc, char **argv)
{
   puts ("test4: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
   fputs ("int -123: ", stdout);
   fputd (-123, stdout);
   puts ("");

   fputs ("long int -123: ", stdout);
   fputld (-123, stdout);
   puts ("");

   fputs ("unsigned -123: ", stdout);
   fputu (-123, stdout);
   puts ("");

   fputs ("long unsigned -123: ", stdout);
   fputlu (-123, stdout);
   puts ("");

   fputs ("long unsigned -123 (base 16): ", stdout);
   fputlx (-123, stdout);
   puts ("");
}

void test5 (int argc, char **argv)
{
   puts ("test5: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

   int ret;
   struct sigaction sa;
   struct sigaction saold;
   char buff[512];
   char *c;

   ret = open ("./main", O_RDONLY, 0);
   printf2 ("test1: sigaction: ret %d\n", ret);

   ret = sigaction (SIGUSR1, &sa, &saold);
   printf2 ("test1: sigaction: ret %d\n", ret);

   ret = sigprocmask (SIG_BLOCK, 0, 0);
   printf2 ("test1: sigprocmask: ret %d\n", ret);

   c = getcwd (buff, 512);
   printf2 ("test1: getcwd: c %p buff %p '%s'\n", c, buff, buff);

   ret = brk (&ret);
   printf2 ("test1: brk: ret %d\n", ret);
}

int main (int argc, char **argv)
{
   __libc_init_poet ();

   test1 (argc, argv);
   test2 (argc, argv);
   test3 (argc, argv);
   test4 (argc, argv);
   test5 (argc, argv);

   return 0;
}