/*  */

#include "pthread.h"

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
/*void *t21(){k=1;}
void *t22(){k=2;}
void *t23(){l=1;}
void *t24(){l=2;}
*/
int main()
{
  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);
  pthread_create(id3, NULL, t3, NULL);
  pthread_create(id4, NULL, t4, NULL);
  pthread_create(id5, NULL, t5, NULL);
  pthread_create(id6, NULL, t6, NULL);
  pthread_create(id7, NULL, t7, NULL);
  pthread_create(id8, NULL, t8, NULL);
  pthread_create(id9, NULL, t9, NULL);
  pthread_create(id10, NULL, t10, NULL);
  pthread_create(id11, NULL, t11, NULL);
  pthread_create(id12, NULL, t12, NULL);
  pthread_create(id13, NULL, t13, NULL);
  pthread_create(id14, NULL, t14, NULL);
  pthread_create(id15, NULL, t15, NULL);
  pthread_create(id16, NULL, t16, NULL);
  pthread_create(id17, NULL, t17, NULL);
  pthread_create(id18, NULL, t18, NULL);
  pthread_create(id19, NULL, t19, NULL);
  pthread_create(id20, NULL, t20, NULL);
/*  pthread_create(id21, NULL, t21, NULL);
  pthread_create(id22, NULL, t22, NULL);
  pthread_create(id23, NULL, t23, NULL);
  pthread_create(id24, NULL, t24, NULL);
*/
}
