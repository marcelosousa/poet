
#include <assert.h>

int main (void)
{
	int x, y;

	x = 1;

	if (y <= 10)
	{
		y = 10;
	}
	else
	{
		while (x < y)
		{
			x = 2 * x;
			y = y - 1;
		}
	}

	x = y + 1;
	assert (x > 0);
}

