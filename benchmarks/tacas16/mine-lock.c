#include "pthread.h"

int y = 0;
int x = 0;
int __poet_mutex_lock=0;

void * thr1 (void * param)
{
  x=1;
  /*
  int cond = 0; //nondet(0,1);
  //int x1;
  //int y1;
  while (cond < 1) {
    //pthread_mutex_lock(__poet_mutex_lock);
    //if (x > 0) {
      x = x+1;
    //pthread_mutex_unlock(__poet_mutex_lock);
    //y=0;
      cond=cond +1;
     // y1 = y;
    //  y = y1-1;
    //}
    
  }*/
}

void * thr2 (void * param)
{
  x=1;
  /*int cond = 0;// int a_cond=0; //nondet(0,1);
  //int x1;
  //int y1;
  while (cond < 1) {
    //pthread_mutex_lock(__poet_mutex_lock);
    //if (x < 4) {
      x = x+1;
    //pthread_mutex_unlock(__poet_mutex_lock);
    //y=0;  
      cond=cond +1;
     // y1 = y;
     //  y = y1+1;
    //}
  
  }*/
}

int main(){
  pthread_t t1,t2;
  //pthread_mutex_init(__poet_mutex_lock, NULL); 
	pthread_create (t1, NULL, thr1, NULL);
	pthread_create (t2, NULL, thr2, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
}

