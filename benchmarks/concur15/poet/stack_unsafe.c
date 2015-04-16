/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/stack_false-unreach-call.c */

/* #define FENCE() asm volatile ("mfence" ::: "memory") */

/* void __VERIFIER_assume(int); */

/* #include <assert.h> */
#include "pthread.h"
//#include <stdio.h>

#define TRUE	  (1)
#define FALSE	  (0) 
#define SIZE	  (5)
#define OVERFLOW  (-1)
#define UNDERFLOW (-2)

int volatile top=0;
unsigned int arr[SIZE];
_Bool flag=FALSE;

volatile int x = 0, y = 0;

/* void lock(int id){ */
/*   if(id){ */
/*     x = 1; */
/*     FENCE(); */
/*     __VERIFIER_assume(y == 0); */
/*   }else{ */
/*     y = 1; */
/*     FENCE(); */
/*     __VERIFIER_assume(x == 0); */
/*   } */
/* }; */

/* void unlock(int id){ */
/*   FENCE(); */
/*   if(id){ */
/*     x = 0; */
/*   }else{ */
/*     y = 0; */
/*   } */
/* }; */

/* void inc_top(void) */
/* { */
/*   top++; */
/* } */

/* void dec_top(void) */
/* { */
/*   top--; */
/* } */

/* int get_top(void) */
/* { */
/*   return top; */
/* } */

/* int push(unsigned int *stack, int x) */
/* { */
/*   if (top==SIZE)  */
/*   { */
/*     printf("stack overflow\n"); */
/*     return OVERFLOW; */
/*   }  */
/*   else  */
/*   { */
/*     stack[get_top()] = x; */
/*     inc_top(); */
/*   } */
/*   return 0; */
/* } */

/* int pop(unsigned int *stack) */
/* { */
/*   if (get_top()==0)  */
/*   { */
/*     printf("stack underflow\n");	 */
/*     return UNDERFLOW; */
/*   }  */
/*   else  */
/*   { */
/*     dec_top(); */
/*     return stack[get_top()];   */
/*   } */
/*   return 0; */
/* } */

void *t1() 
{
  int i;
  unsigned int tmp;
  int push_cond;

  for(i=0; i<SIZE; i=i+1)
  {
    // inline lock(0);
    int id = 0;
    
    if(id){
      x = 1;
    L1: if(y != 0) goto L1;
    }else{
      y = 1;
    L2: if(x!=0) goto L2;
    }
    
    tmp = 42%SIZE;
    
    // inline push 
    if (top==SIZE)
      {
	push_cond = OVERFLOW;
      }
    else
      {
	int Top = top;
	arr[Top] = tmp;
	top = Top + 1;
	push_cond = 0;
      }
    
    if (push_cond == OVERFLOW)
      __poet_fail();//assert(0);
    flag=TRUE;
    // unlock(0);
    if(id){
      x = 0;
    }else{
      y = 0;
    }

  }
}

void *t2(void *arg) 
{
  int i;
  int pop_cond;
  for(i=0; i<SIZE; i= i+1)
  {
    //lock(1);
    int id = 1;
    
    if(id){
      x = 1;
    L3: if(y != 0) goto L3;
    }else{
      y = 1;
    L4: if(x!=0) goto L4;
    }
    if (flag)
    {
      if(top == 0){
	pop_cond = UNDERFLOW;
      }
      else
        {
          int Top = top; 
	  top = Top -1; 
	  //dec_top();
          //return stack[get_top()];
	  pop_cond = arr[top];
        }
            
      if (!(pop_cond!=UNDERFLOW))
        __poet_fail();//assert(0);
    }
    //unlock(1);
    if(id){
      x = 0;
    }else{
      y = 0;
    }

  }
}


int main(void) 
{
  //  #ifndef GOTO
  pthread_t id1, id2;

  pthread_create(id1, NULL, t1, NULL);
  pthread_create(id2, NULL, t2, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);

/*   #else */
/*   __CPROVER_ASYNC_0: t1(NULL); */
/*   t2(NULL); */
/*   #endif */
/* return 0; */
}

