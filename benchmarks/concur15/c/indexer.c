/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/indexer_true-unreach-call.c */

/* BOUND 5 */

//#include <assert.h>
//#include <stdlib.h>
#include "pthread.h"

#define SIZE  8
#define MAX   4
#define NUM_THREADS  2

int volatile table[SIZE];
pthread_mutex_t  cas_mutex[SIZE];

pthread_t  tids[NUM_THREADS];


int cas(int volatile * tab, int h, int val, int new_val)
{
  int ret_val = 0;
  pthread_mutex_lock(&cas_mutex[h]);
  
 
  if ( tab[h] == val ) {
    tab[h] = new_val;
    ret_val = 1;
  }

  pthread_mutex_unlock(&cas_mutex[h]);

  
  return ret_val;
} 



void * t0()
{
  int tid;
  int m = 0;
  int w, h;
  tid = 0;
  
  while(1){
    if ( m < MAX ){
      m = m +1 ;
      w = m * 11 + tid;
    }
    else{
      //pthread_exit(NULL);
      goto END;
    }
    
    h = (w * 7) % SIZE;
    
    //if (h<0)
    // assert(0);

    while ( cas(table, h, 0, w) == 0){
      h = (h+1) % SIZE;
    }
  }
 END: ;
}


int main()
{
  int i, arg;

  //  for (i = 0; i < SIZE; i++)
  pthread_mutex_init(cas_mutex[0], NULL);
  pthread_mutex_init(cas_mutex[1], NULL);
  pthread_mutex_init(cas_mutex[2], NULL);
  pthread_mutex_init(cas_mutex[3], NULL);
  pthread_mutex_init(cas_mutex[4], NULL);
  pthread_mutex_init(cas_mutex[5], NULL);
  pthread_mutex_init(cas_mutex[6], NULL);
  pthread_mutex_init(cas_mutex[7], NULL);


  //  for (i = 0; i < NUM_THREADS; i++){
  //  arg=i;
  pthread_create(tids[0], NULL,  thread_routine0, NULL);
  pthread_create(tids[1], NULL,  thread_routine1, NULL);

    //}

  //  for (i = 0; i < NUM_THREADS; i++){
  pthread_join(tids[0], NULL);
  pthread_join(tids[1], NULL);
    // }

  //return 0;
}
