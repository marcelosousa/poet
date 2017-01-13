int main()
{
  int i = nondet(0,5);
  int j = 0;

  while (j < 10)
  {
    i = i + j;
    j = j + 1;
  }
 
  return i;
}
