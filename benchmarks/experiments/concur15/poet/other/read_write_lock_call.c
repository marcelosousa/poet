/* Testcase from Threader's distribution. For details see:
   http://www.model.in.tum.de/~popeea/research/threader

   This file is adapted from the example introduced in the paper:
   Thread-Modular Verification for Shared-Memory Programs 
   by Cormac Flanagan, Stephen Freund, Shaz Qadeer.
*/

#include "pthread.h"

int w=0, r=0, x, y;

void *writer1() { //writer1
  int aux=w;
  while(aux != 0 || r != 0) { aux = w; } // __VERIFIER_assume(w==0 && r==0);
  w = 1; 
  x = 3;
  w = 0;
}

void *writer2() { //writer2
  int aux=w;
  while(aux != 0 || r != 0) { aux = w; } // __VERIFIER_assume(w==0 && r==0);
  w = 1; 
  x = 3;
  w = 0;
}

void *reader1() { //reader1
  int l;

  int aux=w;
  while(aux != 0){ 
    aux = w; 
  }
  int auxr = r;
  r = auxr+1;

  l = x;
  y = l;
  l = r-1;
  r = l;
}

void *reader2() { //reader2
  int l;

  int aux=w;
  while(aux != 0){ 
    aux = w; 
  }
  int auxr = r;
  r = auxr+1;

  l = x;
  y = l;
  l = r-1;
  r = l;
}

int main() {
  pthread_t t1, t2, t3, t4;
  pthread_create(t1, NULL, writer1, NULL);
  pthread_create(t2, NULL, reader1, NULL);
  pthread_create(t3, NULL, writer2, NULL);
  pthread_create(t4, NULL, reader2, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  pthread_join(t3, NULL);
  pthread_join(t4, NULL);
}
