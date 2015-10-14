
// suggested by Vijay (5. Increment-Decrement family)

#include "pthread.h"

int x = 0;
int y = 0;

void *thr1()
{
	int x2, y2;

	while (y >= 0)
	{
		if (x <= 50)
		{
			y2 = y;
			y = y2 + 1;
		}
		else
		{
			y2 = y;
			y = y2 - 1;
		}
		x2 = x;
		x = x2 + 1;
	}
}
void *thr2()
{
	int x2, y2;

	while (y >= 0)
	{
		if (x <= 50)
		{
			y2 = y;
			y = y2 + 1;
		}
		else
		{
			y2 = y;
			y = y2 - 1;
		}
		x2 = x;
		x = x2 + 1;
	}
}
void *thr3()
{
	int x2, y2;

	while (y >= 0)
	{
		if (x <= 50)
		{
			y2 = y;
			y = y2 + 1;
		}
		else
		{
			y2 = y;
			y = y2 - 1;
		}
		x2 = x;
		x = x2 + 1;
	}
}
void *thr4()
{
	int x2, y2;

	while (y >= 0)
	{
		if (x <= 50)
		{
			y2 = y;
			y = y2 + 1;
		}
		else
		{
			y2 = y;
			y = y2 - 1;
		}
		x2 = x;
		x = x2 + 1;
	}
}
void *thr5()
{
	int x2, y2;

	while (y >= 0)
	{
		if (x <= 50)
		{
			y2 = y;
			y = y2 + 1;
		}
		else
		{
			y2 = y;
			y = y2 - 1;
		}
		x2 = x;
		x = x2 + 1;
	}
}

int main (void)
{
	pthread_t t1, t2, t3, t4, t5;
	int k = 5;

	pthread_create(t1, NULL, thr1, NULL);
	pthread_create(t2, NULL, thr2, NULL);
	pthread_create(t3, NULL, thr3, NULL);
	pthread_create(t4, NULL, thr4, NULL);
	pthread_create(t5, NULL, thr5, NULL);

	pthread_join (t1, NULL);
	pthread_join (t2, NULL);
	pthread_join (t3, NULL);
	pthread_join (t4, NULL);
	pthread_join (t5, NULL);

	// assert (y != -k); // unsafe, there is one interleaving where this is possible
	if (y == -k) __poet_fail ();
}

