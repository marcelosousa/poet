#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define PRINTF2_NORMAL
#include "printf2.h"

#define MAX_QUEUE 5
#define MAX_ITEMS 7

int q[MAX_QUEUE];
int qsiz;
pthread_mutex_t mq;

void queue_init ()
{
   pthread_mutex_init (&mq, NULL);
   qsiz = 0;
}

void queue_insert (int x)
{
   int done = 0;
   int i = 0;
   printf2 ("prod: trying\n");
   while (done == 0)
   {
      i++;
      pthread_mutex_lock (&mq);
      if (qsiz < MAX_QUEUE)
      {
         printf2 ("prod: got it! x %d qsiz %d i %d\n", x, qsiz, i);
         done = 1;
         q[qsiz] = x;
         qsiz++;
      }
      pthread_mutex_unlock (&mq);
   }
}

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

void swap (int *t, int i, int j)
{
   int aux;
   aux = t[i];
   t[i] = t[j];
   t[j] = aux;
}

int findmaxidx (int *t, int count)
{
   int i, mx;
   mx = 0;
   for (i = 1; i < count; i++)
   {
      if (t[i] > t[mx]) mx = i;
   }
   assert (mx < count);
   t[mx] = -t[mx];
   return mx;
}

int source[MAX_ITEMS];
int sorted[MAX_ITEMS];

void producer ()
{
   int i, idx;

   for (i = 0; i < MAX_ITEMS; i++)
   {
      idx = findmaxidx (source, MAX_ITEMS);
      queue_insert (idx);
   }
}

void consumer ()
{
   int i, idx;
   for (i = 0; i < MAX_ITEMS; i++)
   {
      idx = queue_extract ();
      sorted[i] = idx;
   }
}

void *thread (void * arg)
{
   (void) arg;
   producer ();
   return NULL;
}

int main ()
{
   pthread_t t;

   __libc_init_poet ();

#if 1
   // this code initializes the source array with random numbers
   unsigned seed = (unsigned) time (0);
   int i;
   srand (seed);
   printf ("Using seed %u\n", seed);
   for (i = 0; i < MAX_ITEMS; i++)
   {
      source[i] = random() % 20;
      assert (source[i] >= 0);
      printf2 ("source[%d] = %d\n", i, source[i]);
   }
   printf2 ("==============\n");
#endif

   queue_init ();
   pthread_create (&t, NULL, thread, NULL);
   consumer ();
   pthread_join (t, NULL);

#if 1
   // this code prints the sorted array
   printf2 ("==============\n");
   for (i = 0; i < MAX_ITEMS; i++)
      printf2 ("sorted[%d] = %d\n", i, sorted[i]);
#endif
   return 0;
}

