#include "pthread.h"
//#include <stdio.h>

int x = 0;

void *p(void* arg){
    x = 1;
}

void *q(void* arg){
    int* m = &x;
    *m = 2;
}

int main(){
    /* references to the threads */
    pthread_t p_t;
    pthread_t q_t;
    
    /* create the threads and execute */
    pthread_create(&p_t, NULL, p, NULL);
    pthread_create(&q_t, NULL, q, NULL);
    
    /* wait for the threads to finish */
    pthread_join(p_t, NULL);
    pthread_join(q_t, NULL);

//    printf("%d\n", x);
 
    return 0;
}

