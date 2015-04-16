#include <pthread.h>

int N = 32; 
int d = 0; 

pthread_mutex_t l1; 

void *thr1()
{
  int i =0; 
  while (i < N)
    {
      // lock 
      pthread_mutex_lock(&l1); 
      int l = d;
      d = l + i;
      //unlock
      pthread_mutex_unlock(&l1); 
      i = i + 5;
    }

}

void *thr2()
{
  int j =0; 
  while (j < N){
    
    // lock 
    pthread_mutex_lock(&l1); 
    int l = d;
    d = l - j;
    //unlock 
    pthread_mutex_unlock(&l1); 
    j = j + 2; 
  }
}


int main()
{
  pthread_t t1, t2; 

  pthread_mutex_init(&l1, 0); 
  
  pthread_create(&t1, 0, thr1, 0);
  pthread_create(&t2, 0, thr2, 0);

  pthread_join(t1, 0);

  pthread_join(t2, 0);

}
