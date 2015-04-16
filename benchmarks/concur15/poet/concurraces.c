/*  */

#include "pthread.h"

pthread_mutex_t l1; 
pthread_mutex_t l2; 
pthread_mutex_t l3; 
pthread_mutex_t l4; 
pthread_mutex_t l5; 

void *t1(){pthread_mutex_lock(l1);}
void *t2(){pthread_mutex_lock(l1);}
void *t3(){pthread_mutex_lock(l2);}
void *t4(){pthread_mutex_lock(l2);}
void *t5(){pthread_mutex_lock(l3);}
void *t6(){pthread_mutex_lock(l3);}
void *t7(){pthread_mutex_lock(l4);}
void *t8(){pthread_mutex_lock(l4);}
void *t9(){pthread_mutex_lock(l5);}
void *t10(){pthread_mutex_lock(l5);}

int main()
{

  pthread_t id1; 
  pthread_t id2;
  pthread_t id3;
  pthread_t id4;
  pthread_t id5;
  pthread_t id6;
  pthread_t id7;
  pthread_t id8;
  pthread_t id9;
  pthread_t id10;
      
  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);
  pthread_create(id3, NULL, t3, NULL);
  pthread_create(id4, NULL, t4, NULL);
  pthread_create(id5, NULL, t5, NULL);
  pthread_create(id6, NULL, t6, NULL);
  pthread_create(id7, NULL, t7, NULL);
  pthread_create(id8, NULL, t8, NULL);
  pthread_create(id9, NULL, t9, NULL);
  pthread_create(id10, NULL, t10, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
  pthread_join(id3, NULL);
  pthread_join(id4, NULL);
  pthread_join(id5, NULL);
  pthread_join(id6, NULL);
  pthread_join(id7, NULL);
  pthread_join(id8, NULL);
  pthread_join(id9, NULL);
  pthread_join(id10, NULL);
}
