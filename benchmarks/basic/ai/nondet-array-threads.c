#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <assert.h>

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
  srand(time(NULL));
  int num = rand() % 128;

  printf("Spawning %d threads\n", num);

  pthread_mutex_init(&m, NULL);

  pthread_t *ptr;
  ptr = malloc(sizeof(pthread_t)*num);

  for (int i=0; i < num; i++)
    pthread_create(&ptr[i], NULL, inc, NULL);

  for (int i=0; i < num; i++)
    pthread_join(ptr[i], NULL);
 
  assert (x == num);
 
  return 0;
}   
