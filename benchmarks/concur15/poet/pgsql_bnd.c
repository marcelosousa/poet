/* Adapted from PGSQL benchmark from http://link.springer.com/chapter/10.1007%2F978-3-642-37036-6_28 */

/* #define FENCE() asm volatile ("mfence" ::: "memory") */

/* #ifdef ENABLE_PSO_FENCES */
/* #define PSO_FENCE() FENCE() */
/* #else */
/* #define PSO_FENCE() /\*No FENCE*\/ */
/* #endif */

//#include <stdbool.h>
//#include <assert.h>
#include "pthread.h"

#define LOOP 4

volatile int latch1 = 1;
volatile int flag1  = 1;
volatile int latch2 = 0;
volatile int flag2  = 0;

int __unbuffered_tmp2 = 0;

void* worker_1()
{
  int ret;
  for(int i=0;i<LOOP;i=i+1) {
    for(int j=0;!latch1 && j<LOOP;j=j+1);
    if(!latch1)  goto Exit1;//return NULL;
    //assert(!latch1 || flag1);
    if(latch1) __poet_fail();
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
 Exit1: ret =0;
}

void* worker_2()
{
  int ret;
  for(int i=0;i<LOOP;i=i+1) {
    for(int j=0;!latch2 && j<LOOP;j=j+1);
    if(!latch2) goto Exit2;//return NULL;
    //assert(!latch2 || flag2);
    if (latch2) __poet_fail();
    if (!flag2) __poet_fail();
    
    latch2 = 0;
    if(flag2) {
      flag2 = 0;
      flag1 = 1;
      //PSO_FENCE();
      latch1 = 1;
    }
  }
  //return NULL;
 Exit2: ret =0;
}

int main() {
  //  #ifndef GOTO
  pthread_t t1, t2;
  pthread_create(t1, NULL, worker_1, NULL);
  pthread_create(t2, NULL, worker_2, NULL);
  /* #else */
  /* __CPROVER_ASYNC_0: worker_1(NULL); */
  /* worker_2(NULL); */
  /* #endif */
  /* return 0; */
}
