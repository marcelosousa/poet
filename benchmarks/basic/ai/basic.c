#include "pthread.h"

int x = 0;

void *p(void* arg){
  int y = 1;
}


int main(){
  /* references to the threads */
  pthread_t p_t[2];
  
  /* create the threads and execute */
  pthread_create(&p_t[0], NULL, p, NULL);
  pthread_create(&p_t[1], NULL, p, NULL);
  
  /* wait for the threads to finish */
  pthread_join(&p_t[1], NULL);
  pthread_join(&p_t[0], NULL);
 
  return 0;
}

