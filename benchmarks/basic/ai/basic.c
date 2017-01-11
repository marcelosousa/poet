// #include "pthread.h"
#include <pthread.h>

struct Foo {
  int a;
  int b;
};

void* incx()
{
  pthread_exit(NULL); 
}

int main(int argc, char **argv) 
{
  int i = nondet(1,3);
  struct Foo f;
  f.a = i;
  f.b = i;

  pthread_t ptr;

  pthread_create(&ptr, NULL, incx, NULL);
 
  pthread_join(ptr, NULL);

  return 0;
}   
