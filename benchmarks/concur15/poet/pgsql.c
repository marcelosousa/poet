/* Adapted from PGSQL benchmark from http://link.springer.com/chapter/10.1007%2F978-3-642-37036-6_28 */

/* BOUND 8 */

/* #define FENCE() asm volatile ("mfence" ::: "memory") */

/* #ifdef ENABLE_PSO_FENCES */
/* #define PSO_FENCE() FENCE() */
/* #else */
/* #define PSO_FENCE() /\*No FENCE*\/ */
/* #endif */

//#include <stdbool.h>
//#include <assert.h>
#include "pthread.h"

//void __VERIFIER_assume(int);

volatile int latch1 = 1;
volatile int flag1  = 1;
volatile int latch2 = 0;
volatile int flag2  = 0;

int __unbuffered_tmp2 = 0;

void* worker_1()
{
  for(;;) {
    // __VERIFIER_assume(latch1);
  L1: if(latch1 != 1) goto L1;
    //assert(!latch1 || flag1);
    if (latch1) __poet_fail();
    if(!flag1) __poet_fail();

    latch1 = 0;
    if(flag1) {
      flag1 = 0;
      flag2 = 1;
      //PSO_FENCE();
      latch2 = 1;
    }
  }
  //return NULL;
}

void* worker_2()
{
  for(;;) {
    //    __VERIFIER_assume(latch2);
  L2: if(!latch2) goto L2;
    
    //    assert(!latch2 || flag2);
    if(latch2) __poet_fail();
    if(!flag2) __poet_fail();
    latch2 = 0;
    if(flag2) {
      flag2 = 0;
      flag1 = 1;
      //      PSO_FENCE();
      latch1 = 1;
    }
  }
  //return NULL;
}

int main() {
  //  #ifndef GOTO
  pthread_t t1;
  pthread_t t2;
  pthread_create(t1, NULL, worker_1, NULL);
  pthread_create(t2, NULL, worker_2, NULL);
  //  #else
    // __CPROVER_ASYNC_0: worker_1(NULL);
  //worker_2(NULL);
  // #endif
  //return 0;
}
