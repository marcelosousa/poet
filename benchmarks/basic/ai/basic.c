// #include <time.h>
// #include <stdlib.h>
// #include <stdio.h>
#include "pthread.h"
// #include <assert.h>

pthread_mutex_t m;
int x=0;

void* incx()
{
  pthread_mutex_lock(&m); 
    x++;
  pthread_mutex_unlock(&m);
  pthread_exit(NULL); 
}

int main(int argc, char **argv) 
{
  int a = 0, b = 5;

  for (int i = 0; i <= b; i++) {
    a = i;
  }

  pthread_mutex_init(&m, NULL);
  pthread_t ptr;

  pthread_create(&ptr, NULL, incx, NULL);
  pthread_mutex_lock(&m); 
  x = a;
  pthread_mutex_unlock(&m);
 
  pthread_join(ptr, NULL);

  if (x > a+1) {
    poet_error();
  }
 
  return 0;
}   
