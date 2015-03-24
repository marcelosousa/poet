/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread-atomic/szymanski_true-unreach-call.c */

#ifdef ENABLE_PSO_FENCES
#define ENABLE_TSO_FENCES
#endif

#define FENCE() asm volatile ("mfence" ::: "memory")

#ifdef ENABLE_TSO_FENCES
#define TSO_FENCE() FENCE()
#else
#define TSO_FENCE() /*No FENCE*/
#endif

#ifdef ENABLE_PSO_FENCES
#define PSO_FENCE() FENCE()
#else
#define PSO_FENCE() /*No FENCE*/
#endif

void __VERIFIER_assume(int);

/* Testcase from Threader's distribution. For details see:
   http://www.model.in.tum.de/~popeea/research/threader
*/

#include <pthread.h>
#include <assert.h>

volatile int flag1 = 0, flag2 = 0; // integer flags 
volatile int x; // boolean variable to test mutual exclusion

void *thr1(void *arg) {
  flag1 = 1;
  TSO_FENCE();
  __VERIFIER_assume(flag2 < 3);
  flag1 = 3;
  TSO_FENCE();
  if (flag2 == 1) {
    flag1 = 2;
    TSO_FENCE();
    __VERIFIER_assume(flag2 == 4);
  }
  flag1 = 4;
  TSO_FENCE();
  __VERIFIER_assume(flag2 < 2);
  // begin critical section
  x = 0;
  assert(x<=0);
  PSO_FENCE();
  // end critical section
  __VERIFIER_assume(2 > flag2 || flag2 > 3);
  flag1 = 0;
  return NULL;
}

void *thr2(void *arg) {
  flag2 = 1;
  TSO_FENCE();
  __VERIFIER_assume(flag1 < 3);
  flag2 = 3;
  TSO_FENCE();
  if (flag1 == 1) {
    flag2 = 2;
    TSO_FENCE();
    __VERIFIER_assume(flag1 == 4);
  }
  flag2 = 4;
  TSO_FENCE();
  __VERIFIER_assume(flag1 < 2);
  // begin critical section
  x = 1;
  assert(x>=1);
  PSO_FENCE();
  // end critical section
  __VERIFIER_assume(2 > flag1 || flag1 > 3);
  flag2 = 0;
  return NULL;
}

int main() {
  #ifndef GOTO
  pthread_t t1, t2;
  pthread_create(&t1, 0, thr1, 0);
  pthread_create(&t2, 0, thr2, 0);
  pthread_join(t1, 0);
  pthread_join(t2, 0);
  #else
  __CPROVER_ASYNC_0: thr1(NULL);
  thr2(NULL);
  #endif
  return 0;
}
