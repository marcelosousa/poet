#define MAX   6
#define MIN   0

int buf=0;

void *prod1()
{
  int c1=0;
  while(c1==0){
    if(buf < MAX){
      buf++;
    }
  }
}


void *prod2()
{
  int c2=0;
  while(c2==0){
    if(buf < MAX){
      buf = buf + 2;
    }
  }
}


void *cons()
{
  int c3=0; 
  int local=0; 
  while(1){
     local = buf;
     if(local > MIN){
       buf--; 
     }

     while(local > 0) {
       local--;
     }
  }
}

int main()
{

  int id1; 
  int id2;
  int id3;

  pthread_create(id1, NULL, prod1, NULL);
  pthread_create(id2, NULL, prod2, NULL);
  pthread_create(id3, NULL, cons, NULL);
  return 0;
}
