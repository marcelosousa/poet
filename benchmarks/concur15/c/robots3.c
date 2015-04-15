/* Adapted from Cartersian POR paper */
#include "pthread.h"

#define SIZE 12

int array[SIZE][SIZE];

void *robot1() {
    int x = 0;
    int y = 0;
    int dirX = 1;
    int dirY = 1;
    
    while(1){
        array[x][y] = 0;
        x=x+dirX;
        y=y+dirY;
        if(x==SIZE-1 || x==0){
            dirX = dirX * (-1);
        }
        if(y==SIZE-1 || y==0){
            dirY = dirY * (-1);
        }
        if(!(array[x][y] == 1 || (x == 9 || x==2))){
            __poet_fail();
        }
        array[x][y] = 1;
    }
}

void *robot2() {
    int x = 4;
    int y = 0;
    int dirX = 1;
    int dirY = 1;
    
    while(1){
        array[x][y] = 0;
        x=x+dirX;
        y=y+dirY;
        if(x==SIZE-1 || x==0){
            dirX = dirX * (-1);
        }
        if(y==SIZE-1 || y==0){
            dirY = dirY * (-1);
        }
        if(!(array[x][y] == 1 || (x == 9 || x==2))){
            __poet_fail();
        }
        array[x][y] = 1;
    }
}

void *robot3() {
    int x = 7;
    int y = 0;
    int dirX = 1;
    int dirY = 1;
    
    while(1){
        array[x][y] = 0;
        x=x+dirX;
        y=y+dirY;
        if(x==SIZE-1 || x==0){
            dirX = dirX * (-1);
        }
        if(y==SIZE-1 || y==0){
            dirY = dirY * (-1);
        }
        if(!(array[x][y] == 1 || (x == 9 || x==2))){
            __poet_fail();
        }
        array[x][y] = 1;
    }
}

int main() {

  pthread_t t1;
  pthread_t t2;
  pthread_t t3;

  pthread_create(t1, NULL, robot1, NULL);
  pthread_create(t2, NULL, robot2, NULL);
  pthread_create(t3, NULL, robot3, NULL);
  
  pthread_join(t1, NULL);
  pthread_join(t2, NULL);
  pthread_join(t3, NULL);

}
