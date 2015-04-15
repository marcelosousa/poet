
int x =1; 


int main()
{
  int i; 
  int t; 
  
  for(i =0; i < 5; i = i+1)
    {  
      if(i < 2)
	{
	  t = x;
	  x = t*2; 
	}
      else{
	t = x;
	x = t+2; 
      }
    }
}
