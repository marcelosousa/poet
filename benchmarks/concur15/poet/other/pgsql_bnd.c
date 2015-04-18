/* Adapted from PGSQL benchmark from http://link.springer.com/chapter/10.1007%2F978-3-642-37036-6_28 */

#include "pthread.h"

#define LOOP 4

int latch1 = 1;
int flag1  = 1;
int latch2 = 0;
int flag2  = 0;

int __unbuffered_tmp2 = 0;

void* worker_1()
{
  int ret;
  int i=0;
  int j=0;
  while(i<LOOP){
    while(!latch1 && j<LOOP){ j=j+1; }
    if(!latch1)  goto Exit1; //return NULL;
    
    if(latch1){
      if(!flag1){
        __poet_fail(); //assert(!latch1 || flag1);
      }
    }

    latch1 = 0;
    if(flag1) {
      flag1 = 0;
      flag2 = 1;
      latch2 = 1;
    }
  i=i+1;
  }
 Exit1: ret =0;
}

void* worker_2()
{
  int ret;
  int i=0;
  int j=0;
  while(i<LOOP){
    while(!latch2 && j<LOOP){ j=j+1; }
    if(!latch2) goto Exit2; //return NULL;
    
    if (latch2){ 
      if (!flag2){
        __poet_fail(); //assert(!latch2 || flag2);
      }
    }
    
    latch2 = 0;
    if(flag2) {
      flag2 = 0;
      flag1 = 1;
      latch1 = 1;
    }
  }
 Exit2: ret =0;
}

int main() {
  pthread_t t1, t2;
  pthread_create(t1, NULL, worker_1, NULL);
  pthread_create(t2, NULL, worker_2, NULL);
}
