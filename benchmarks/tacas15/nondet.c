
#include <assert.h>

int main (void)
{
	int x = 0;
	int y;

	if (y)
	{
		x = 10;
	}
	else
	{
		x = 20;
	}

	assert (x > 0);
}
