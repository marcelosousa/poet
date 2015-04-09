#include "pthread.h"
    
int x=0;

void *p(){
    l=0;
//    x = 1;
}

void *q(){
    l=0;
//    y = 2;
}

int main(){
    /* references to the threads */
    //pthread_t p_t;
    //pthread_t q_t;
    
    /* create the threads and execute */
    pthread_create(p_t, NULL, p, NULL);
    pthread_create(q_t, NULL, q, NULL);
    
    /* wait for the threads to finish */
    pthread_join(p_t, NULL);
    pthread_join(q_t, NULL);
}
