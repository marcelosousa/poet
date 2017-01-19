#include "pthread.h"

#define MAX_QUEUE 5
#define MAX_ITEMS 7

int q[MAX_QUEUE];
int qsiz;
int source[MAX_ITEMS];
int sorted[MAX_ITEMS];
pthread_mutex_t mq;

void *thread (void * arg)
{
   // INLINE producer (); BEGIN
   int i, idx;
   // INLINE findmaxidx (); BEGIN 
   int j, mx;
   // INLINE findmaxidx (); END 
   // INLINE queue_insert (idx); BEGIN
   int done = 0;
   // INLINE queue_insert (idx); END 

   for (i = 0; i < MAX_ITEMS; i++)
   {
      // INLINE findmaxidx (source, MAX_ITEMS); BEGIN
      mx = 0;
      for (j = 1; j < MAX_ITEMS; j++)
      {
         if (source[j] > source[mx]) {
           mx = j;
         }
      }
      if (mx >= MAX_ITEMS){
        poet_error ();
      }
      source[mx] = -source[mx];
      idx = mx;
      // INLINE findmaxidx (source, MAX_ITEMS); END

      if (idx >= MAX_ITEMS) {
        poet_error ();
      }

      // INLINE queue_insert (idx); BEGIN
      done = 0;
      while (done == 0)
      {
         pthread_mutex_lock (&mq);
         if (qsiz < MAX_QUEUE)
         {
            done = 1;
            q[qsiz] = idx;
            qsiz++;
         }
         pthread_mutex_unlock (&mq);
      }
      // INLINE queue_insert (idx); END 
   }
   // INLINE producer (); END 
   
   pthread_exit(NULL);
}

int main ()
{
   pthread_t t;

   // INLINE idx = queue_extract (); BEGIN
   int done, x, k;
   // INLINE idx = queue_extract (); END 

   // this code initializes the source array with random numbers
   int i;
   int j, idx;
   for (i = 0; i < MAX_ITEMS; i++)
   {
      source[i] = nondet(0,20);
      if (source[i] < 0) {
         poet_error ();
      }
   }

   // INLINE queue_init (); BEGIN
   pthread_mutex_init (&mq, NULL);
   qsiz = 0;
   // INLINE queue_init (); END 

   pthread_create (&t, NULL, thread, NULL);

   // INLINE consumer (); BEGIN
   for (j = 0; i < MAX_ITEMS; i++)
   {
      // INLINE idx = queue_extract (); BEGIN
      done = 0; x = 0; k = 0;
      while (done == 0)
      {
         pthread_mutex_lock (&mq);
         if (qsiz > 0)
         {
            done = 1;
            x = q[0];
            qsiz--;
            for (k = 0; k < qsiz; k++) q[k] = q[k+1]; // shift left 1 elem
         }
         pthread_mutex_unlock (&mq);
      }
      idx = x;
      // INLINE idx = queue_extract (); BEGIN

      sorted[j] = idx;
      if (idx < 0) {
        poet_error ();
      }
      if (idx >= MAX_ITEMS) {
        poet_error ();
      }
   }
   // INLINE consumer (); END

   pthread_join (t, NULL);
   return 0;
}
