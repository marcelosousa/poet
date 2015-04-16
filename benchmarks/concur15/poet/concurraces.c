/*  */

#include "pthread.h"

int x=0;
int y=0;
int z=0;

void *t1()
{
    x=1;
}

void *t2()
{
    x=2;
}


void *t3()
{
    y=1;
}

void *t4()
{
    y=2;
}

/*
void *t5()
{
    z=1;
}

void *t6()
{
    z=2;
}
*/
int main()
{

  pthread_t id1; 
  pthread_t id2;
  pthread_t id3;
  pthread_t id4;
//  pthread_t id5;
//  pthread_t id6;
      
  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);
  pthread_create(id3, NULL, t3, NULL);
  pthread_create(id4, NULL, t4, NULL);
//  pthread_create(id5, NULL, t5, NULL);
//  pthread_create(id6, NULL, t6, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);
  pthread_join(id3, NULL);
  pthread_join(id4, NULL);
//  pthread_join(id5, NULL);
//  pthread_join(id6, NULL);
}
