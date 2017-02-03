#include <pthread.h>
#include <assert.h>

#define K 6

int a[K];
int i = 0;

void *counter()
{
 for (int x = 1; x < K; x++)
 {
   i = x; 
 }
}

void *wa(void *aux)
{
 int x = *((int *) aux);
 a[x] = x;
}

int main()
{
  pthread_t idk;
  pthread_t idw[K];

  pthread_create(&idk, NULL, counter, NULL);
  for (int x = 0; x < K; x++)
  {
    int *arg = malloc(sizeof(*arg));
    *arg = x;
    pthread_create(&idw[x], 0, wa, arg);
  }

  assert (a[i] >= 0);

  for (int x = 0; x < K; x++)
    pthread_join(idw[x],NULL);
  pthread_join(idk,NULL);
}
