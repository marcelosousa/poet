int main()
{
  int i = nondet(0,5);
  int j = 0;

  if (i > 1) {
    j = 1;
  } else {
    j = -1;
  }
  
  return j;
}
