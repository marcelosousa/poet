/*  
 * POET Regression Suite 
 * Test 3: SSB 
*/
#include <pthread.h>

int x=0;
int y=0;
int z=0;

void *p(){
    x = 1;
}

void *q(){
    z = 7;
}

void *r(){
    if ( z == 0){
      x = 1; 
   }
}

void *s(){
    y = z;
}

int main(){
    /* references to the threads */
    pthread_t p_t;
    pthread_t q_t;
    pthread_t r_t;
    pthread_t s_t;

    /* create the threads and execute */
    pthread_create(&p_t, NULL, p, NULL);
    pthread_create(&q_t, NULL, q, NULL);
    pthread_create(&r_t, NULL, r, NULL);
    pthread_create(&s_t, NULL, s, NULL);

    /* wait for the threads to finish */
    pthread_join(p_t, NULL);
    pthread_join(q_t, NULL);
    pthread_join(r_t, NULL);
    pthread_join(s_t, NULL);
}
