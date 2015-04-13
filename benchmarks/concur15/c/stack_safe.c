/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/stack_true-unreach-call.c */

//#define FENCE() asm volatile ("mfence" ::: "memory")

//void __VERIFIER_assume(int);

//#include <assert.h>
#include <pthread.h>
//#include <stdio.h>

#define TRUE	  (1)
#define FALSE	  (0) 
#define SIZE	  (5)
#define OVERFLOW  (-1)
#define UNDERFLOW (-2)

int volatile top=0;
unsigned int arr[SIZE];

volatile int x = 0, y = 0;

/* void lock(int id){ */
/*   if(id){ */
/*     x = 1; */
/*     //   FENCE(); */
/*     // __VERIFIER_assume(y == 0); */
/*   }else{ */
/*     y = 1; */
/*     // FENCE(); */
/*     // __VERIFIER_assume(x == 0); */
/*   } */
/* }; */

/* void unlock(int id){ */
/*   //  FENCE(); */
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
/*     //printf("stack overflow\n"); */
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
/*   if (top==0)  */
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
  int id; 
  int push_cond;
  int Top; 

  for(i=0; i<SIZE; i++)
  {
    id =0;
    
    // inlined lock code
    if(id){
      x = 1;
    }else{
      y = 1;
    }
   
    tmp = 42%SIZE;
    
    // inlined push code 
    if (top==SIZE)
      {
	//printf("stack overflow\n");
	push_cond = OVERFLOW;
      }
    else
      {
	Top = top;
	arr[Top] = tmp;
	top = Top +1;
      }
    // push_cond = 0;
    
    if (push_cond == OVERFLOW)
      __poet_false; //assert(0);
    
    // inlined unlock code
    //unlock(0);
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
  int id;
  int pop_cond;
  int Top; 

  id =1;

  for(i=0; i<SIZE; i++)
  {
    //lock(1);
    // inlined lock code
    if(id){
      x = 1;
    }else{
      y = 1;
    }

    if (top>0)
    {    
      if (top==0)
	{
	  pop_cond = UNDERFLOW;
	}
      else
	{
	  Top = top; 
	  top = Top - 1;
	  //return stack[get_top()];
	}
      if (pop_cond==UNDERFLOW)
	__poet_false;
    }    
    //unlock(1);
     // inlined unlock code
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

