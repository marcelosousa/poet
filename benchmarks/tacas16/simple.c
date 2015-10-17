#include "pthread.h"

int y = 0;
int x = 0;

void * thr1 (void * param)
{
  int cond = 0; int a_cond=0;
  int x1;
  while (cond < 1) {
      x1 = x;
      x = x1+1;
      a_cond = cond;
      cond=a_cond +1;
  }
}

void * thr2 (void * param)
{
  int cond = 0;
  int a_cond=0;
  int x1;
  int y1;
  while (cond < 1) {
      x1 = x;
      x = x1+1;
      a_cond = cond;
      cond=a_cond +1;
  }
}

int main(){
  pthread_t t1,t2;

	pthread_create (t1, NULL, thr1, NULL);
	pthread_create (t2, NULL, thr2, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
	
	
}

