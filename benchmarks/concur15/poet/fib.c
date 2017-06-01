/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/fib_bench_true-unreach-call.c */

#include "pthread.h"

int i=1; 
int j=1;

// #define NUM 11
#define NUM 9

pthread_mutex_t l;

void *t1()
{
  for (int k = 0; k < NUM; k++){
    // pthread_mutex_lock(l);
    i += j;
    // pthread_mutex_unlock(l);
  }
}

void *t2()
{
  for (int k = 0; k < NUM; k++){
    // pthread_mutex_lock(l);
    j += i;
    // pthread_mutex_unlock(l);
  }
}

int
main()
{
  pthread_t id1;
  pthread_t id2;

  // pthread_mutex_init(l, NULL);

  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);

  pthread_join(id1,NULL);
  pthread_join(id2,NULL);

  int l=i;
  if (i > 46368 || j > 46368) {
  // if (i >= 377 || j >= 377) {
      __VERIFIER_error();
  } 
}
