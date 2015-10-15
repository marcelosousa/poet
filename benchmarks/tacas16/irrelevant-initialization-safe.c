
#include "pthread.h"

#define MAX 10

int start = 0;
int x = 0;
int k = MAX;

void * thr1 (void * param)
{
	int k2; //private copy of k

	k2 = k;
	x = k2;
	start = 1;
}

void * thr2 (void * param)
{
	int k2; //private copy of k

	k2 = k;
	x = -k2;
	start = 1;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	int i, x2, k2;

	pthread_create (t1, NULL, thr1, NULL);
	pthread_create (t2, NULL, thr2, NULL);

	// wait until one of the threads terminate
	while (start != 1) i = 0;

	i = 0;
	x2 = x;
	while (i < x2 * x2) // original condition: i < x * x
	{
		i = i + 1;
		x2 = x;
	}

	if (i < 0) __poet_fail (); // assert (i >= 0); // holds, safe
	k2 = k;
	if (i != k2 * k2) __poet_fail (); // assert(i == k*k); // safe
}

