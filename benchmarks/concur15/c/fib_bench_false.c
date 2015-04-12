/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/fib_bench_false-unreach-call.c */

//#include <assert.h>

#include "pthread.h"

volatile int i;
volatile int j;

#define NUM 5

void *t1()
{
  int k = 0;
  int t;
  int s;
  for (k = 0; k < NUM; k=k+1){
    t = j;
    s = i + t; 
    i= s;
  }
}

void *t2()
{
  int k = 0;
  int t;
  int s;
  for (k = 0; k < NUM; k=k+1){
    t = j;
    s = t + i;
    j = s;
  }
  // pthread_exit(NULL);
}

int main()
{

  pthread_t id1; 
  pthread_t id2;
  
  i =1;
  j =1;
  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);

  // svs: add poet_fail call here
  /* if (i >= 144 || j >= 144) { */
  /*   assert(0); */
  /* } */
  pthread_join(id1, NULL);
  pthread_join(id2, NULL);

  //  return 0;
}
