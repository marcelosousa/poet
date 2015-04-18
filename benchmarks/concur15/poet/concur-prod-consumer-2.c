/*  */

#include "pthread.h"

#define MAX   6
#define MIN   0

int buf_1=0;
int buf_2=0;
pthread_mutex_t l1;
pthread_mutex_t l2;
int buf_3=0;
int buf_4=0;
pthread_mutex_t l3;
pthread_mutex_t l4;

void *prod1()
{
    int aux=0;
    while(1){
        pthread_mutex_lock(l1);
        if(buf_1 < MAX){
            aux=buf_1;
            buf_1=aux+1;
            aux=0;
        }
        pthread_mutex_unlock(l1);         
    }
}


void *prod2()
{
    int aux=0;
    while(1){
        pthread_mutex_lock(l2);
        if(buf_2 < MAX){
            aux=buf_2;
            buf_2=aux+1;
            aux=0;
        }
        pthread_mutex_unlock(l2);         
    }
}


void *cons()
{
    int aux=0;
    while(1){
       pthread_mutex_lock(l1);
       if(buf_1 > MIN){
           aux=buf_1;
           buf_1=aux-1;
//           aux=0;
       }
       pthread_mutex_unlock(l1); 
       pthread_mutex_lock(l2);
       if(buf_2 > MIN){
           aux=buf_2;
           buf_2=aux-1;
//           aux=0;
       }
       pthread_mutex_unlock(l2); 
    }
}

void *prod3()
{
    int aux=0;
    while(1){
        pthread_mutex_lock(l3);
        if(buf_3 < MAX){
            aux=buf_3;
            buf_3=aux+1;
            aux=0;
        }
        pthread_mutex_unlock(l3);         
    }
}


void *prod4()
{
    int aux=0;
    while(1){
        pthread_mutex_lock(l4);
        if(buf_4 < MAX){
            aux=buf_4;
            buf_4=aux+1;
            aux=0;
        }
        pthread_mutex_unlock(l4);         
    }
}


void *cons2()
{
    int aux=0;
    while(1){
       pthread_mutex_lock(l3);
       if(buf_3 > MIN){
           aux=buf_3;
           buf_3=aux-1;
       }
       pthread_mutex_unlock(l3); 
       pthread_mutex_lock(l4);
       if(buf_4 > MIN){
           aux=buf_4;
           buf_4=aux-1;
       }
       pthread_mutex_unlock(l4); 
    }
}

int main()
{

  pthread_t id1; 
  pthread_t id2;
  pthread_t id3;
  pthread_t id4; 
  pthread_t id5;
  pthread_t id6;

  pthread_mutex_init(l1, NULL); 
  pthread_mutex_init(l2, NULL); 
  pthread_mutex_init(l3, NULL); 
  pthread_mutex_init(l4, NULL); 
  
  pthread_create(id1, NULL, prod1, NULL);
  pthread_create(id2, NULL, prod2, NULL);
  pthread_create(id3, NULL, cons, NULL);
  pthread_create(id4, NULL, prod3, NULL);
  pthread_create(id5, NULL, prod4, NULL);
  pthread_create(id6, NULL, cons2, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
  pthread_join(id3, NULL);
  pthread_join(id4, NULL);
  pthread_join(id5, NULL);
  pthread_join(id6, NULL);
}
