/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/stack_true-unreach-call.c */

#define FENCE() asm volatile ("mfence" ::: "memory")

void __VERIFIER_assume(int);

#include <assert.h>
#include <pthread.h>
#include <stdio.h>

#define TRUE	  (1)
#define FALSE	  (0) 
#define SIZE	  (5)
#define OVERFLOW  (-1)
#define UNDERFLOW (-2)

int volatile top=0;
unsigned int arr[SIZE];

volatile int x = 0, y = 0;

void lock(int id){
  if(id){
    x = 1;
    FENCE();
    __VERIFIER_assume(y == 0);
  }else{
    y = 1;
    FENCE();
    __VERIFIER_assume(x == 0);
  }
};

void unlock(int id){
  FENCE();
  if(id){
    x = 0;
  }else{
    y = 0;
  }
};

void inc_top(void)
{
  top++;
}

void dec_top(void)
{
  top--;
}

int get_top(void)
{
  return top;
}

int push(unsigned int *stack, int x)
{
  if (top==SIZE) 
  {
    printf("stack overflow\n");
    return OVERFLOW;
  } 
  else 
  {
    stack[get_top()] = x;
    inc_top();
  }
  return 0;
}

int pop(unsigned int *stack)
{
  if (top==0) 
  {
    printf("stack underflow\n");	
    return UNDERFLOW;
  } 
  else 
  {
    dec_top();
    return stack[get_top()];  
  }
  return 0;
}

void *t1(void *arg) 
{
  int i;
  unsigned int tmp;

  for(i=0; i<SIZE; i++)
  {
    lock(0);   
    tmp = 42%SIZE;
    if ((push(arr,tmp)==OVERFLOW))
      assert(0);
    unlock(0);
  }
}

void *t2(void *arg) 
{
  int i;

  for(i=0; i<SIZE; i++)
  {
    lock(1);
    if (top>0)
    {    
      if ((pop(arr)==UNDERFLOW))
        assert(0);
    }    
    unlock(1);
  }
}


int main(void) 
{
  #ifndef GOTO
  pthread_t id1, id2;

  pthread_create(&id1, NULL, t1, NULL);
  pthread_create(&id2, NULL, t2, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);

  #else
  __CPROVER_ASYNC_0: t1(NULL);
  t2(NULL);
  #endif
return 0;
}

