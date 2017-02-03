#include <pthread.h>
#include <assert.h>

#define K 8

pthread_mutex_t ma[K];
pthread_mutex_t mi;
int a[K];
int i = 0;

void *counter()
{
 for (int x = 1; x < K; x++)
 {
   pthread_mutex_lock(&mi);
     i = x; 
   pthread_mutex_unlock(&mi);
 }
}

void *wa(void *aux)
{
 int x = *((int *) aux);
 pthread_mutex_lock(&ma[x]);
   a[x] = x;
 pthread_mutex_unlock(&ma[x]);
}

int main()
{
  pthread_t idk;
  pthread_t idw[K];
  pthread_mutex_init(&mi, NULL);

  for (int x = 0; x < K; x++)
  {
    int *arg = malloc(sizeof(*arg));
    *arg = x;
    pthread_mutex_init(&ma[x], NULL);
    pthread_create(&idw[x], 0, wa, arg);
  }

  pthread_create(&idk, NULL, counter, NULL);

  int idx = 0;
  pthread_mutex_lock (&mi);
    idx = i;
  pthread_mutex_unlock (&mi);

  pthread_mutex_lock (&ma[idx]);
    assert (a[idx] >= 0);
  pthread_mutex_unlock (&ma[idx]);

  for (int x = 0; x < K; x++)
    pthread_join(idw[x],NULL);
  pthread_join(idk,NULL);
}
