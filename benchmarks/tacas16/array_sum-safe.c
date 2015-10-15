
#include "pthread.h"

#define SIZE	10

int vec1[SIZE];
// int vec2[SIZE]; // we don't use it

int vec1_sum;
int vec2_sum;

void *thr1() {
	int i;
	int acc = 0;

	for (i = 0; i < SIZE; i = i + 1)
	{
		assert (i < SIZE);
		acc = acc + vec1[i];
	}

	vec1_sum = acc;
}

void *thr2() {
	int i;
	int acc = 0;

	for (i = 0; i < SIZE; i = i + 1)
	{
		assert (i < SIZE);
		acc = acc + vec1[i];
	}

	vec2_sum = acc;
}

int main (void)
{
	int i;
	pthread_t t1, t2;

	for (i = 0; i < SIZE; i = i + 1) vec1[i] = nondet (1, 3); // nondet (i, 2*i);
	//for (i = 0; i < SIZE; i++) vec2[i] = nondet (1, 3);

	pthread_create(t1, NULL, thr1, NULL);
	pthread_create(t2, NULL, thr2, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);

	// sum = n * (n-1) / 2
	int sum_nondet_eq_1 = (SIZE * (SIZE - 1)) / 2;

	// assert (vec1_sum >= sum_nondet_eq_1);
	if (vec1_sum < sum_nondet_eq_1) __poet_fail ();
	//assert (vec1_sum <= 3 * sum_nondet_eq_1);
	if (vec1_sum > 3 * sum_nondet_eq_1) __poet_fail ();

	// assert (vec2_sum >= sum_nondet_eq_1);
	if (vec2_sum < sum_nondet_eq_1) __poet_fail ();
	//assert (vec2_sum <= 3 * sum_nondet_eq_1);
	if (vec2_sum > 3 * sum_nondet_eq_1) __poet_fail ();
}

