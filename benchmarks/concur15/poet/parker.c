/* A recreation of the bug
 * http://bugs.sun.com/view_bug.do?bug_id=6822370
 *
 * based on the description in
 * https://blogs.oracle.com/dave/entry/a_race_in_locksupport_park
 */

/* BOUND 10 */

/* void __VERIFIER_assume(int); */

#include "pthread.h"
//#include <assert.h>

/* Testing */
volatile int global_cond = 0; // Some global condition variable which the parker will wait for
volatile int unparker_finished = 0; // Flag indicating that the unparker thread has finished

/* Small low-level mutex implementation */
volatile int x = 0, y = 0;

/* void lock(int id){ */
/*   if(id){ */
/*     x = 1; */
/*     //FENCE(); */
/*     // __VERIFIER_assume(y == 0); */
/*   }else{ */
/*     y = 1; */
/*     //  FENCE(); */
/*     // __VERIFIER_assume(x == 0); */
/*   } */
/* } */

/* void unlock(int id){ */
/*   //FENCE(); */
/*   if(id){ */
/*     x = 0; */
/*   }else{ */
/*     y = 0; */
/*   } */
/* } */

/* Returns 0 on success, nonzero on failure. */
/* int trylock(int id){ */
/*   if(id){ */
/*     x = 1; */
/*     // FENCE(); */
/*     if(y == 0){ */
/*       //return 0; */
/*     }else{ */
/*       x = 0; */
/*       // return 1; */
/*     } */
/*   }else{ */
/*     y = 1; */
/*     //FENCE(); */
/*     if(x == 0){ */
/*       //return 0; */
/*     }else{ */
/*       y = 0; */
/*       //return 1; */
/*     } */
/*   } */
/* } */

/* The parker */
volatile int parker_counter;
volatile int parker_cond;

/* void parker_cond_signal(){ */
/*   parker_cond = 0; */
/* } */

/* void parker_cond_wait(int id){ */
/*   parker_cond = 1; */
/*   unlock(id); */
/*   assert(!unparker_finished || parker_cond == 0); // Otherwise wait forever */
/*   __VERIFIER_assume(parker_cond == 0); */
/*   lock(id); */
/* } */

/* void parker_unpark(){ */
/*   lock(0); */
/*   int s = parker_counter; */
/*   parker_counter = 1; */
/*   unlock(0); */
/*   if(s < 1){ */
/*     parker_cond_signal(); */
/*   } */
/* } */

/* void parker_park(){ */
/*   if(parker_counter > 0){ */
/*     parker_counter = 0; */
/*     PSO_FENCE(); */
/*     return; */
/*   } */
/*   if(trylock(1) != 0){ */
/*     return; */
/*   } */
/*   if(parker_counter > 0){ */
/*     parker_counter = 0; */
/*     unlock(1); */
/*     return; */
/*   } */
/*   parker_cond_wait(1); */
/*   parker_counter = 0; */
/*   unlock(1); */
/* } */

/* Testing */

void *parker(){
  int i =0; 
  while(!global_cond){
    // inline parker_park();
    if(parker_counter > 0){
      parker_counter = 0;
      // PSO_FENCE();
      //return;
      goto END;
    }
    // inline trylock (1) 
    int id = 1; 
    int trylock_retval;
    if(id){
      x = 1;
      // FENCE();
      if(y == 0){
	//return 0;
	trylock_retval = 0;
      }else{
	x = 0;
	// return 1;
	trylock_retval = 1;
      }
    }else{
      y = 1;
      //FENCE();
      if(x == 0){
	//return 0;
	trylock_retval = 0;
      }else{
	y = 0;
	//return 1;
	trylock_retval = 1;
      }
    }    
    //    if(trylock(1)!= 0){
    if(trylock_retval == 0){
      //      return;
      goto END;
    }
    if(parker_counter > 0){
      parker_counter = 0;
      // inline  unlock(1);
      int id1 = 1;
      if(id){
	x = 0;
      }else{
	y = 0;
      }
      //    return;
      goto END;
    }
    //inline  parker_cond_wait(1);
    int id2 = 1;
    parker_cond = 1;
    //unlock(id2);
    if(id2){
      x = 0;
    }else{
      y = 0;
    }
    if (unparker_finished)
      if (parker_cond!=0)
	__poet_fail();//assert(!unparker_finished || parker_cond == 0); // Otherwise wait forever
    
    //__VERIFIER_assume(parker_cond == 0);
    // inline  lock(id);
    int id3 =1;
    if(id3){
      x = 1;
    }else{
      y = 1;
    }  
    parker_counter = 0;
    //  unlock(1);
    if(id3){
      x = 0;
    }else{
      y = 0;
    }
    
  }
  
  //  return NULL;
 END: i = 0;
}

/* void *unparker(){ */
/*   //  parker_unpark(); */
/*   //lock(0); */
/*   int id =1; */
/*   if(id){ */
/*     x = 1; */
/*   }else{ */
/*     y = 1; */
/*   } */
/*   int s = parker_counter; */
/*   parker_counter = 1; */
/*   //  unlock(0); */
/*   if(id){ */
/*     x = 0; */
/*   }else{ */
/*     y = 0; */
/*   } */

/*   if(s < 1){ */
/*     //    parker_cond_signal(); */
/*     parker_cond = 0; */
/*   } */

/*   global_cond = 1; */
/*   //  FENCE(); */
/*   //  parker_unpark(); */
/*   int id1 =1; */
/*   if(id1){ */
/*     x = 1; */
/*   }else{ */
/*     y = 1; */
/*   } */
/*   int s = parker_counter; */
/*   parker_counter = 1; */
/*   //  unlock(0); */
/*   if(id1){ */
/*     x = 0; */
/*   }else{ */
/*     y = 0; */
/*   } */

/*   if(s < 1){ */
/*     //    parker_cond_signal(); */
/*     parker_cond = 0; */
/*   } */
  
/*   // Done */
/*   // FENCE(); */
/*   unparker_finished = 1; */

/*   //return NULL; */
/* } */

int main(){

  parker_counter = 0;
  parker_cond = 0;
  //  #ifndef GOTO
  pthread_t t1;
  pthread_create(t1,NULL,parker,NULL);
  //#else
  //__CPROVER_ASYNC_0: parker(NULL);
  //#endif
  //  unparker(NULL);
    int id =1;
  if(id){
    x = 1;
  }else{
    y = 1;
  }
  int s = parker_counter;
  parker_counter = 1;
  //  unlock(0);
  if(id){
    x = 0;
  }else{
    y = 0;
  }

  if(s < 1){
    //    parker_cond_signal();
    parker_cond = 0;
  }

  global_cond = 1;
  //  FENCE();
  //  parker_unpark();
  int id1 =1;
  if(id1){
    x = 1;
  }else{
    y = 1;
  }
  int s1 = parker_counter;
  parker_counter = 1;
  //  unlock(0);
  if(id1){
    x = 0;
  }else{
    y = 0;
  }

  if(s1 < 1){
    //    parker_cond_signal();
    parker_cond = 0;
  }
  
  // Done
  // FENCE();
  unparker_finished = 1;

  
  pthread_join(t1, NULL);
  //return 0;
}
