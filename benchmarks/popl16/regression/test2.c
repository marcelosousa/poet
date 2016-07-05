/* Adapted from: https://svn.sosy-lab.org/software/sv-benchmarks/trunk/c/pthread/fib_bench_true-unreach-call.c */

int i=1; 

void *t1()
{
  i=1;
}

int
main()
{
  int id1;
  pthread_create(id1, NULL, t1, NULL);
  i=2;

}
