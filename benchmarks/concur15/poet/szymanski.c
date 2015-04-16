/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread-atomic/szymanski_true-unreach-call.c */

/* void __VERIFIER_assume(int); */

/* Testcase from Threader's distribution. For details see:
   http://www.model.in.tum.de/~popeea/research/threader
*/

#include "pthread.h"
//#include <assert.h>

int flag1 = 0;
int flag2 = 0; // integer flags 
int x; // boolean variable to test mutual exclusion

void *thr1() {
  int aux=0;
  flag1 = 1;
  busy_1: if (flag2 >= 3) { goto busy_1; } //__VERIFIER_assume(flag2 < 3);
  flag1 = 3;
  if (flag2 == 1) {
    flag1 = 2;
    busy_2: if (flag2 != 4) { goto busy_2; } // __VERIFIER_assume(flag2 == 4);
  }
  flag1 = 4;
  busy_3: if (flag2 >= 2) { goto busy_3; } //__VERIFIER_assume(flag2 < 2);
  // begin critical section
  x = 0;
  if(x > 0)
    __poet_fail();
  // end critical section
  busy_4: 
    aux=flag2;
    if(2 <= aux && flag2 <= 3) { goto busy_4; } //__VERIFIER_assume(2 > flag2 || flag2 > 3);
  flag1 = 0;
}

void *thr2() {
  int aux=0;
  flag2 = 1;
  busy_21: if (flag1 >= 3) { goto busy_21; } /* __VERIFIER_assume(flag1 < 3); */
  flag2 = 3;
  if (flag1 == 1) {
    flag2 = 2;
    busy_22: if (flag1 != 4) { goto busy_22; } /* __VERIFIER_assume(flag1 == 4); */
  }
  flag2 = 4;
  busy_23: if (flag1 >= 2) { goto busy_23; } /* __VERIFIER_assume(flag1 < 2); */
  // begin critical section
  x = 1;
  if ( x<1) 
    __poet_fail();
  // end critical section
  busy_24:
    aux=flag1;
    if(2 <= aux && flag1 <= 3) { goto busy_24; } //  __VERIFIER_assume(2 > flag1 || flag1 > 3);
  flag2 = 0;
}

int main() {

  pthread_t t1;
  pthread_t t2;
  pthread_create(t1, NULL, thr1, NULL);
  pthread_create(t2, NULL, thr2, NULL);
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);

}
