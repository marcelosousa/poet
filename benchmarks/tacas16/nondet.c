
#include "pthread.h"

int main (void)
{
	int x = nondet (-1, -10);
	unsigned y = nondet (10, 20);

	if (x >= 15) __poet_fail ();

}
