// #include <time.h>
// #include <stdlib.h>
// #include <stdio.h>
#include "pthread.h"
// #include <assert.h>

pthread_mutex_t m[2];
int x=0;
// int y=0;

void* incx()
{
  pthread_mutex_lock(&m[0]); 
    x++;
  pthread_mutex_unlock(&m[0]);
  pthread_exit(NULL); 
}

void* incy()
{
  pthread_mutex_lock(&m[1]); 
    x = 2;
  pthread_mutex_unlock(&m[1]);
  pthread_exit(NULL); 
}

int main(int argc, char **argv) 
{

  pthread_mutex_init(&m[0], NULL);
  pthread_mutex_init(&m[1], NULL);

  pthread_t ptr[2];

  pthread_create(&ptr[0], NULL, incx, NULL);
  pthread_create(&ptr[1], NULL, incy, NULL);

  pthread_join(ptr[0], NULL);
  pthread_join(ptr[1], NULL);
  x++; 
  // assert (x == num);
 
  return 0;
}   
