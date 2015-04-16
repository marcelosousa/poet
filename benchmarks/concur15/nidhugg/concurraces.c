/*  */

#include <pthread.h>

int a=0;
int b=0;
int c=0;
int d=0;
int e=0;
int f=0;
int g=0;
int h=0;
int i=0;
int j=0;
int k=0;
int l=0;
int m=0;
int n=0; 

void *t1(){a=1;}
void *t2(){a=2;}
void *t3(){b=1;}
void *t4(){b=2;}
void *t5(){c=1;}
void *t6(){c=2;}
void *t7(){d=1;}
void *t8(){d=2;}
void *t9(){e=1;}
void *t10(){e=2;}
void *t11(){f=1;}
void *t12(){f=2;}
void *t13(){g=1;}
void *t14(){g=2;}
void *t15(){h=1;}
void *t16(){h=2;}
void *t17(){i=1;}
void *t18(){i=2;}
void *t19(){j=1;}
void *t20(){j=2;}
void *t21(){k=1;}
void *t22(){k=2;}
void *t23(){l=1;}
void *t24(){l=2;}

int main()
{
  pthread_t id1,id2,id3,id4,id5,id6,id7,id8,id9,id10,id11,id12,id13,id14,id15,id16,id17,id18,id19,id20,id21,id22,id23,id24;

  pthread_create(&id1, 0, t1, 0);
  pthread_create(&id2, 0, t2, 0);
  pthread_create(&id3, 0, t3, 0);
  pthread_create(&id4, 0, t4, 0);
  pthread_create(&id5, 0, t5, 0);
  pthread_create(&id6, 0, t6, 0);
  pthread_create(&id7, 0, t7, 0);
  pthread_create(&id8, 0, t8, 0);
  pthread_create(&id9, 0, t9, 0);
  pthread_create(&id10, 0, t10, 0);
  pthread_create(&id11, 0, t11, 0);
  pthread_create(&id12, 0, t12, 0);
  pthread_create(&id13, 0, t13, 0);
  pthread_create(&id14, 0, t14, 0);
  pthread_create(&id15, 0, t15, 0);
  pthread_create(&id16, 0, t16, 0);
  pthread_create(&id17, 0, t17, 0);
  pthread_create(&id18, 0, t18, 0);
  pthread_create(&id19, 0, t19, 0);
  pthread_create(&id20, 0, t20, 0);
/*  pthread_create(&id21, 0, t21, 0);
  pthread_create(&id22, 0, t22, 0);
  pthread_create(&id23, 0, t23, 0);
  pthread_create(&id24, 0, t24, 0);
*/
}
