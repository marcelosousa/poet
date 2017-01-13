#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <assert.h>

#define N 8

pthread_mutex_t m;
int x=0;

void* inc()
{
  pthread_mutex_lock(&m); 
    x++;
  pthread_mutex_unlock(&m);
  pthread_exit(NULL); 
}


int main(int argc, char **argv) 
{

  pthread_mutex_init(&m, NULL);

  pthread_t ptr[N];

  for (int i=0; i < N; i++)
    pthread_create(&ptr[i], NULL, inc, NULL);

  for (int i=0; i < N; i++)
    pthread_join(ptr[i], NULL);
 
  assert (x == N);
 
  return 0;
}   
