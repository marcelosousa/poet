#include "pthread.h"

#define MAX_QUEUE 5
#define MAX_ITEMS 7

int q[MAX_QUEUE];
int source[MAX_ITEMS];
int qsiz;
pthread_mutex_t mq;

void *queue_insert (void * arg)
{
   int done = 0;
   int i = 0;
   int x = 1;

   qsiz = MAX_QUEUE;
   while (done == 0)
   {
      pthread_mutex_lock (&mq);
      if (qsiz < MAX_QUEUE)
      {
         done = 1;
         q[qsiz] = x;
         qsiz++;
      }
      pthread_mutex_unlock (&mq);
   }
   pthread_exit(NULL);
}

#if 0
int queue_extract ()
{
   int done = 0;
   int x, i = 0;
   printf2 ("consumer: trying\n");
   while (done == 0)
   {
      i++;
      pthread_mutex_lock (&mq);
      if (qsiz > 0)
      {
         done = 1;
         x = q[0];
         printf2 ("consumer: got it! x %d qsiz %d i %d\n", x, qsiz, i);
         qsiz--;
         for (i = 0; i < qsiz; i++) q[i] = q[i+1]; // shift left 1 elem
      }
      pthread_mutex_unlock (&mq);
   }
   return x;
}
#endif

int main ()
{
   pthread_t t;

#if 1
   // this code initializes the source array with random numbers
   for (int i = 0; i < MAX_ITEMS; i++)
   {
      source[i] = nondet(0,20);
      if (source[i] < 0) {
        poet_error();
      }
   }
#endif

   pthread_create (&t, NULL, queue_insert, NULL);
   pthread_join (t, NULL);

#if 0
   // this code prints the sorted array
   printf2 ("==============\n");
   for (i = 0; i < MAX_ITEMS; i++)
      printf2 ("sorted[%d] = %d\n", i, sorted[i]);
#endif
   return 0;
}

