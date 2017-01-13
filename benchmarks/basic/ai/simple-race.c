#include "pthread.h"

int x = 0;

void *p(void* arg){
  x = x + 1;
}

int main1(){
  /* references to the threads */
  pthread_t p_t;
  
  x = 2;
  /* create the threads and execute */
  pthread_create(&p_t, NULL, p, NULL);
  
  x = 3;
 
  /* wait for the threads to finish */
  pthread_join(&p_t, NULL);
  
  x = 4; 
  return 0;
}

int main2(){
  /* references to the threads */
  pthread_t p_t;
  pthread_t q_t;
  
  /* create the threads and execute */
  pthread_create(&p_t, NULL, p, NULL);
  pthread_create(&q_t, NULL, p, NULL);
  
  /* wait for the threads to finish */
  pthread_join(&p_t, NULL);
  pthread_join(&q_t, NULL);
  
  return 0;
}

