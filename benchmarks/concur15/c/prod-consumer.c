#include "pthread.h"

#define SIZE 3

#define MAXVAL 1000

int buffer[SIZE];
pthread_mutex_t buff_lock; 

void *p1()
{
  int i; 
  int j = 1;
  while(1) {
    if(j >= MAXVAL){
      j = 1;
    }
    for(i=0; i< SIZE; i = i+1){
      if(buffer[i] == 0){
	pthread_mutex_lock(buff_lock); 
	buffer[i] = j;
	pthread_mutex_unlock(buff_lock);
	j = j +1; 
      }
    }
  }
}

void *c1()
{
  int i; 
  while (1){
    for (i = SIZE-1; i >=0 ; i= i-1){
      //check if buffer[i] is non zero
      if (buffer[i] !=0) {
	pthread_mutex_lock(buff_lock);
	//consume the data by setting it to zero
	buffer[i] = 0;
	pthread_mutex_unlock(buff_lock);
      }
    }
  }
}

void *c2()
{
  int i; 
  while (1){
    for (i = SIZE-1; i >=0 ; i= i-1){
      //check if buffer[i] is non zero
      if (buffer[i] !=0) {
	pthread_mutex_lock(buff_lock);
	//consume the data by setting it to zero
	buffer[i] = 0;
	pthread_mutex_unlock(buff_lock);
      }
    }
  }
}

int main()
{

  pthread_t producer;
  pthread_t consumers[2];
  
  int i; 
  
  // initialize the global buffer 
  memset(buffer, 0, sizeof(int)*SIZE);

  pthread_mutex_init(buff_lock, NULL);

  pthread_create(producer, NULL, p1, NULL);
  pthread_create(consumers[0],  NULL, c1, NULL);
  pthread_create(consumers[1],  NULL, c2, NULL);



  pthread_join(producer, NULL); 
  pthread_join(consumers[0], NULL); 
  pthread_join(consumers[1], NULL); 


  // return 0; 
}
