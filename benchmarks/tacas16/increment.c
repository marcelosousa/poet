int x = 0;
int y = 0;

void* th1(void *arg)
{
  int cond=0; int a_cond=0;
  int x1;
  while (cond < 8) {
    x1 = x;
    x = x + 1;
    a_cond = cond;
    cond = cond+1;
  }
}

void* th2(void *arg)
{
  int cond=0; int a_cond=0;
  int x1;
  while (cond < 8) {
    x1 = x;
    x = x1 + 1;
    a_cond = cond;
    cond = a_cond+1;
  }
}
int main()
{

  pthread_create(thr1, NULL, th1, NULL);
  pthread_create(thr2, NULL, th2, NULL);
  pthread_join(thr1, NULL);
  pthread_join(thr2, NULL);
  if (x<1) __poet_fail ();
}