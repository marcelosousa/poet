/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/fib_bench_true-unreach-call.c */

int i=1; 
int main()
{ 
  int b=0;
  int l=1; 
  while (b==0) {
   l = 0;
  }
}
