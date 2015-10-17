#include "pthread.h"

int x = 0;
int y = 0;

void * thr1 (void * param)
{
  int cond = nondet(0,1);
  int x1;
  int y1;
  while (cond == 1) {
    y1 = y;
    if (x < y1) {
      x1 = x;
      x = x1+1;
    }
  }
}

void * thr2 (void * param)
{
  int cond = nondet(0,1);
  int aux;
  int y1;
  while (y < 20) {
    aux = nondet(1,3);
    y1 = y;
    y = y1 + aux;
  }
}

int main(){
	pthread_t t1;
	pthread_t t2;

	pthread_create (t1, NULL, thr1, NULL);
	pthread_create (t2, NULL, thr2, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
}

