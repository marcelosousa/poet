#include "pthread.h"

int arr[5];

void* incx()
{
  pthread_exit(NULL); 
}

int main(int argc, char **argv) 
{
  int i = nondet(1,3);
  arr[i] = 5;

  pthread_t ptr;

  pthread_create(&ptr, NULL, incx, NULL);
 
  pthread_join(ptr, NULL);

  return 0;
}   
