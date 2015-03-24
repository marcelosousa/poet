/* Adapted from PGSQL benchmark from http://link.springer.com/chapter/10.1007%2F978-3-642-37036-6_28 */

#define FENCE() asm volatile ("mfence" ::: "memory")

#ifdef ENABLE_PSO_FENCES
#define PSO_FENCE() FENCE()
#else
#define PSO_FENCE() /*No FENCE*/
#endif

#include <stdbool.h>
#include <assert.h>
#include <pthread.h>

#define LOOP 4

volatile _Bool latch1 = true;
volatile _Bool flag1  = true;
volatile _Bool latch2 = false;
volatile _Bool flag2  = false;

_Bool __unbuffered_tmp2 = false;

void* worker_1(void* arg)
{
  for(int i=0;i<LOOP;i++) {
    for(int j=0;!latch1 && j<LOOP;j++);
    if(!latch1) return NULL;
    assert(!latch1 || flag1);
    latch1 = false;
    if(flag1) {
      flag1 = false;
      flag2 = true;
      PSO_FENCE();
      latch2 = true;
    }
  }
  return NULL;
}

void* worker_2(void* arg)
{
  for(int i=0;i<LOOP;i++) {
    for(int j=0;!latch2 && j<LOOP;j++);
    if(!latch2) return NULL;
    assert(!latch2 || flag2);
    latch2 = false;
    if(flag2) {
      flag2 = false;
      flag1 = true;
      PSO_FENCE();
      latch1 = true;
    }
  }
  return NULL;
}

int main() {
  #ifndef GOTO
  pthread_t t1, t2;
  pthread_create(&t1, 0, worker_1, NULL);
  pthread_create(&t2, 0, worker_2, NULL);
  #else
  __CPROVER_ASYNC_0: worker_1(NULL);
  worker_2(NULL);
  #endif
  return 0;
}
