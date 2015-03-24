/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread-atomic/lamport_true-unreach-call.c */

/* BOUND 8 */

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

volatile int x, y;
volatile int b1, b2; // boolean flags
volatile int X; // boolean variable to test mutual exclusion

void *thr1(void *arg) {
  while (1) {
    b1 = 1;
    x = 1;
    TSO_FENCE();
    if (y != 0) {
      b1 = 0;
      __VERIFIER_assume(y == 0);
      continue;
    }
    y = 1;
    TSO_FENCE();
    if (x != 1) {
      b1 = 0;
      __VERIFIER_assume(b2 < 1);
      if (y != 1) {
        __VERIFIER_assume(y == 0);
	continue;
      }
    }
    goto breaklbl;
  }
  assume(0);
 breaklbl:
  // begin: critical section
  X = 0;
  assert(X <= 0);
  PSO_FENCE();
  // end: critical section
  y = 0;
  b1 = 0;
  return NULL;
}

void *thr2(void *arg) {
  while (1) {
    b2 = 1;
    x = 2;
    TSO_FENCE();
    if (y != 0) {
      b2 = 0;
      __VERIFIER_assume(y == 0);
      continue;
    }
    y = 2;
    TSO_FENCE();
    if (x != 2) {
      b2 = 0;
      __VERIFIER_assume(b1 < 1);
      if (y != 2) {
         __VERIFIER_assume(y == 0);
	continue;
      }
    }
    goto breaklbl;
  }
  assume(0);
 breaklbl:
  // begin: critical section
  X = 1;
  assert(X >= 1);
  PSO_FENCE();
  // end: critical section
  y = 0;
  b2 = 0;
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
