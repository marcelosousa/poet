#include "pthread.h"

int x = 0;

void *p(void* arg){
  x = 1;
}


int main(){
  /* references to the threads */
  pthread_t p_t[2];
  int i = 0; 
  /* create the threads and execute */
  pthread_create(&p_t[i], NULL, p, NULL);
  i++;
  pthread_create(&p_t[i], NULL, p, NULL);
  i = 0;
   
  x=2;
  /* wait for the threads to finish */
  pthread_join(&p_t[i], NULL);
  i++;
  pthread_join(&p_t[i], NULL);
  return 0;
}

