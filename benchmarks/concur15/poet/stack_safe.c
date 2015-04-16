/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/stack_true-unreach-call.c */

//#define FENCE() asm volatile ("mfence" ::: "memory")

//void __VERIFIER_assume(int);

//#include <assert.h>
#include "pthread.h"
//#include <stdio.h>

/* #define TRUE	  1 */
/* #define FALSE	  0  */
#define SIZE	  5
#define OVERFLOW  -1
#define UNDERFLOW -2

int  top=0;
unsigned int arr[SIZE];

int  x = 0;
int  y = 0;

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
  
  for(i=0; i<SIZE; i=i+1)
  {
    id =0;
    
    // inlined lock code
    if(id){
      x = 1;
    Loop1: if(y!=0) goto Loop1;
    }else{
      y = 1;
    Loop2: if(x!=0) goto Loop2;
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
    
    if (push_cond == OVERFLOW){
      //    __poet_false; //assert(0);
      goto thr1_exit; 
    }
    // inlined unlock code
    //unlock(0);
    if(id){
      x = 0;
    }else{
      y = 0;
    }

  }
 thr1_exit: i = 0;
}

void *t2() 
{
  int i;
  int id;
  int pop_cond;
  int Top; 

  id =1;

  for(i=0; i<SIZE; i=i+1)
  {
    //lock(1);
    // inlined lock code
    if(id){
      x = 1;
    Loop3: if(y!=0) goto Loop3;      
    } 
    else{
      y = 1;
    Loop4: if(x!=0) goto Loop4;      
    }
    
    if (top>0){
      Top = top; 
      top = Top - 1;
    }
    else { // pop_cond == UNDERFLOW
      //	__poet_false;
      goto thr2_exit;
    }
    
    //unlock(1);
     // inlined unlock code
    if(id){
      x = 0;
    }else{
      y = 0;
    }
  }
 thr2_exit: i =0;
}



int main() 
{
  //  #ifndef GOTO
  pthread_t id1;
  pthread_t id2;

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

