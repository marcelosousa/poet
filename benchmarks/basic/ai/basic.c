#include "pthread.h"

int x = 0;

void *p(void* arg){
  x = 1;
}


int main(){
  /* references to the threads */
  pthread_t p_t;
  
  /* create the threads and execute */
  pthread_create(&p_t, NULL, p, NULL);
  
  /* wait for the threads to finish */
  pthread_join(p_t, NULL);
 
  return 0;
}

