/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/fib_bench_true-unreach-call.c */

#include <assert.h>

#include <pthread.h>
#ifdef GOTO
int __unbuffered_cnt=0;
#endif

volatile int i=1, j=1;

#define NUM 5

void *
t1(void* arg)
{
  int k = 0;

  for (k = 0; k < NUM; k++)
    i+=j;

  #ifndef GOTO
  pthread_exit(NULL);
  #else
  __unbuffered_cnt++;
  #endif
}

void *
t2(void* arg)
{
  int k = 0;

  for (k = 0; k < NUM; k++)
    j+=i;

  #ifndef GOTO
  pthread_exit(NULL);
  #else
  __unbuffered_cnt++;
  #endif
}

int
main(int argc, char **argv)
{
  #ifndef GOTO
  pthread_t id1, id2;

  pthread_create(&id1, NULL, t1, NULL);
  pthread_create(&id2, NULL, t2, NULL);
  pthread_join(id1,NULL);
  pthread_join(id2,NULL);
  #else
  __CPROVER_ASYNC_0: t1(NULL);
  __CPROVER_ASYNC_1: t2(NULL);
  __CPROVER_assume(__unbuffered_cnt==2);
  #endif

  if (i > 144 || j > 144) {
    assert(0);
  }

  return 0;
}
