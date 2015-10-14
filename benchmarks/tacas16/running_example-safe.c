
#include "pthread.h"

int x;

void *thr1() {
	int x2;

	if (x >= -1)
	{
		x2 = x;
		x = x2 + 1;
	}
}

void *thr2() {
	int x2;

	if (x != 0)
	{
		x2 = x;
		x = x2 + 1;
	}
}

int main (void)
{
	pthread_t t1, t2;

	x = nondet(-2,1); // x is between -2 and 1, in the collecting semantics {-2,-1,0,1}

	pthread_create(t1, NULL, thr1, NULL);
	pthread_create(t2, NULL, thr2, NULL);
}

