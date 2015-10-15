
#include "pthread.h"

#define MAX 10

int x = MAX;
int y = MAX;

void * p (void * param)
{
	int y2; // private copy of y
	int x2; // private copy of x

	/* decrements x while it is positive and asserts that y >= x; the other
	 * thread will read x at some point and set y = x, so the assertion is
	 * never violated */
	while (x > 0)
	{
		y2 = y;
		if (y2 < x) __poet_fail (); // assert (y2 >= x); // safe
		x2 = x;
		x = x2 - 1;
	}
}

void * t (void * param)
{
	int x2;

	x2 = x;
	y = x2;
}

int main(){
	pthread_t pid;
	pthread_t tid;

	pthread_create (pid, NULL, p, NULL);
	pthread_create (tid, NULL, t, NULL);
	pthread_join(pid, NULL);
	pthread_join(tid, NULL);
}

