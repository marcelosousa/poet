/* Source:
 * D. C. Schmidt, T. Harrison, Double-checked locking,
 * Pattern languages of program design, 1997
 */

/* BOUND 7 */

#define FENCE() asm volatile ("mfence" ::: "memory")

#ifdef ENABLE_PSO_FENCES
#define PSO_FENCE() FENCE()
#else
#define PSO_FENCE() /*No FENCE*/
#endif

#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

void __VERIFIER_assume(int);

struct singleton_t{
  volatile int data; // data == 1 if initialized
};

struct singleton_t *volatile _instance = NULL;

/* Work around using real malloc, to avoid spurious error from cbmc. */
static struct singleton_t theobject;
struct singleton_t *malloc_singleton(){
  static int called = 0;
  assert(!called);
  called = 1;
  return &theobject;
};

#ifndef NDEBUG
volatile int init_count = 0;
#endif

volatile int x = 0, y = 0;

void lock(int id){
  FENCE();
  if(id){
    x = 1;
    FENCE();
    __VERIFIER_assume(y == 0);
  }else{
    y = 1;
    FENCE();
    __VERIFIER_assume(x == 0);
  }
};

void unlock(int id){
  FENCE();
  if(id){
    x = 0;
  }else{
    y = 0;
  }
};

/* Get a pointer to the singleton instance. Create instance at most
 * once. */
struct singleton_t *instance(int id){

  if(_instance == NULL){
    lock(id);
    if(_instance == NULL){
      struct singleton_t *tmp = malloc_singleton();
      tmp->data = 1;
      PSO_FENCE();
      _instance = tmp;
      assert(++init_count == 1);
    }
    unlock(id);
  }

  return (struct singleton_t*)_instance;

};

void *user(void *_arg){
  /* Each user just accesses the singleton instance a number of
   * times. */

  int id = *(int*)_arg;

  int i;
  while(1){
    struct singleton_t *tmp = instance(id);
    assert(tmp);
    assert(tmp->data == 1);
  }
  return NULL;
};

int main(int argc, char *argv[]){
  int ids[2] = {0,1};
  int i;
  #ifndef GOTO
  pthread_t threads[2];
  pthread_create(&threads[0],NULL,user,&ids[0]);
  pthread_create(&threads[1],NULL,user,&ids[1]);
  pthread_join(threads[0],NULL);
  pthread_join(threads[1],NULL);
  #else
  __CPROVER_ASYNC_0: user(&ids[0]);
  user(&ids[1]);
  #endif
  return 0;
};
