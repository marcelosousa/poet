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

void * thr0()
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
    
    if (h<0){
      __poet_fail();   // assert(0);
    }
    // cas functon inlined
    int ret_val = 0;
    pthread_mutex_lock(cas_mutex[h]);
    if ( table[h] == 0 ) {
      table[h] = w;
      ret_val = 1;
    }
    pthread_mutex_unlock(cas_mutex[h]);

    while ( ret_val == 0){
      h = (h+1) % SIZE;
      
      // cas functon inlined
      ret_val = 0;
      pthread_mutex_lock(cas_mutex[h]);
      if ( table[h] == 0 ) {
	table[h] = w;
	ret_val = 1;
      }
      pthread_mutex_unlock(cas_mutex[h]);
    }
  }
 END: tid = 0;
}

void * thr1()
{
  int tid;
  int m = 0;
  int w, h;
  tid = 1;
  
  while(1){
    if ( m < MAX ){
      m = m +1 ;
      w = m * 11 + tid;
    }
    else{
      goto END;
    }
    
    h = (w * 7) % SIZE;
    
    if (h<0){
      __poet_fail();   // assert(0);
    }
    // cas functon inlined
    int ret_val = 0;
    pthread_mutex_lock(cas_mutex[h]);
    if ( table[h] == 0 ) {
      table[h] = w;
      ret_val = 1;
    }
    pthread_mutex_unlock(cas_mutex[h]);
    
    while ( ret_val == 0){
      h = (h+1) % SIZE;
      // cas functon inlined
      ret_val = 0;
      pthread_mutex_lock(cas_mutex[h]);
      if ( table[h] == 0 ) {
	table[h] = w;
	ret_val = 1;
      }
      pthread_mutex_unlock(cas_mutex[h]);
    }
  }
 END: tid = 1;
}




int main()
{
  //  int i, arg;
  pthread_t  tid1; 
  pthread_t  tid2; 
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
  pthread_create(tid1, NULL,  thr0, NULL);
  pthread_create(tid2, NULL,  thr1, NULL);

    //}

  //  for (i = 0; i < NUM_THREADS; i++){
  pthread_join(tid1, NULL);
  pthread_join(tid2, NULL);
    // }

  //return 0;
}
