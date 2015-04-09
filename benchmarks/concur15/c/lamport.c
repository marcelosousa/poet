/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread-atomic/lamport_true-unreach-call.c */

/* Testcase from Threader's distribution. For details see:
   http://www.model.in.tum.de/~popeea/research/threader
*/

#include "pthread.h"


volatile int x, y;
volatile int b1, b2; // boolean flags
volatile int X; // boolean variable to test mutual exclusion

void *thr1() {
  while (1) {
    b1 = 1;
    x = 1;
    if (y != 0) {
      b1 = 0;
      //      continue;
      goto __before_loop;
    }
    y = 1;
    if (x != 1) {
      b1 = 0;
      if (y != 1) {
	//	continue;
	goto __before_loop;
      }
    }
    goto breaklbl;
  __before_loop:;
  }
 breaklbl:
  // begin: critical section
  X = 0;
  //  assert(X <= 0);
  // end: critical section
  y = 0;
  b1 = 0;
}

void *thr2() {
  while (1) {
    b2 = 1;
    x = 2;
    if (y != 0) {
      b2 = 0;
      //      continue;
      goto __before_loop1;
    }
    y = 2;
    if (x != 2) {
      b2 = 0;
      if (y != 2) {
	//	continue;
	goto __before_loop1;
      }
    }
    goto breaklbl;
  __before_loop1:;
  }
 breaklbl:
  // begin: critical section
  X = 1;
  //  assert(X >= 1);
  // end: critical section
  y = 0;
  b2 = 0;
}

int main() {

  pthread_t t1, t2;

  pthread_create(t1, NULL, thr1, NULL);
  pthread_create(t2, NULL, thr2, NULL);

  pthread_join(t1, NULL);
  pthread_join(t2, NULL);

}
