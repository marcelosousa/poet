#include "pthread.h"

int h = 0;
int c = 0;

void * thr1 (void * param)
{
  int cond = nondet(0,1);
  int h1;
  while (cond == 1) {
    if (h < 5) {
      h1 = h;
      h = h1+1;
    }
  }
}

void * thr2 (void * param)
{
  int cond = nondet(0,1);
  while (cond == 1) {
    c1 = h;
    c = c1;
  }
}

void * thr3 (void * param)
{
  int cond = nondet(0,1);
  int l = 0;
  int t = 0;
  while (cond == 1) {
    if (cond==1) {
      t = 0;
    } else {
      t = t + (c - l);
    }
    t = c;
  }
}

int main(){
  pthread_t t1,t2,t3;
	pthread_create (t1, NULL, thr1, NULL);
	pthread_create (t2, NULL, thr2, NULL);
	pthread_create (t3, NULL, thr3, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
	pthread_join(t3, NULL);
}

