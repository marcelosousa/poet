

#include <stdio.h>

struct test1 {
	int i;
	char str[10];
};

struct test2
{
	int i;
	float f;
	void *p;
	struct test1 t;
};

typedef struct test2 test2_t;

void f1 () {
	printf ("hello world\n");
}
void f2 (void) {
	printf ("hello world\n");
}
int f3 (int a, char b, float c) {
	return a + b + (int) c;
}
int *f4 (int *ptr) {
	return ptr;
}

void test1 () {
	// types and expressions
	//
	// variables: declaration
	// pointers: declaration
	// arrays: declaration
	// casts
	// pointer arithmetic
	// array and pointer access
	// arithmetic operators
	// logic operators
	// bitwise operators

	// variables: declaration
	char          c1 = 123;
	signed char   c2 = 123;
	unsigned char c3 = 123;

	int               i1 = 123;
	signed int        i2 = 123;
	unsigned int      i3 = 123;
	long int          i4 = 123;
	long signed int   i5 = 123;
	long unsigned int i6 = 123;
	long long unsigned i7 = 777;
	long              i8;
	long long         i9;

	float       f1 = 123.123;
	double      f2 = 123.123;
	long double f3 = 123.123;

	// modifiers
	volatile int i10;

	// pointers: declaration
	void   *ptr1 = (void*) &i1;
	char   *ptr2 = (char*) &c1;
	int    *ptr3 = (int*) &i2;
	int   **ptr4;
	char ***ptr5;
	long double ***ptr6;

	// arrays: declaration
	char *tab1[1];
	char *tab2[2];
	int   tab3[2];
	int   tab4[4] = { 0, 1, 2, 3 };
	int   tab5[4ull];
	int  *tab6[4ull][3u];
	char  tab7[128] = "hello world, on the first 34 bytes";

	// structs: declaration
	struct test1 s1 = { 123, "hello" };
	struct test2 s2 = { 1, 3.0, &f1, { i2, "hello" }};
	test2_t      s3;
	
	// casts
	c1 = 4;
	c1 = (int) 4ull;
	c1 = (int) 0x12;

	i3 = (unsigned) i2;
	i3 = (unsigned) i2;
	i9 = i3;

	f1 = i2;
	f1 = (float) (long) i2;

	// pointer arithmetic
	ptr2 = tab7 + 4;
	ptr2 = tab7 + c1 * 2;
	ptr2 = ((char *) &i1) + 2;
	ptr3 = &tab3[3]; // beyond last element, but still legal to get address

	// array and pointer access
	i2 = tab4[3];
	i2 = *(tab4 + 3);
	i2 = *(tab4 + tab4[2] - 1);

	// arithmetic operators
	i1 = 3 + 4;
	i1 = 3 / tab4[1] * 2;
	i1++;
	++i1;
	--i1;
	i2--;

	// logic operators
	c3 = 4 && i1;
	c3 = 4 || i1;
	c3 = ! i2;
	c3 = ! i2;

	// bitwise operators
	i1 = 7 & i2;
	i1 = 8 | i2;
	i1 = 8 ^ i2;
	i1 = ~1;
	c1 = ~1;
	c2 = ~1;
}

void test2 () {
	// control flow
	//
	// if
	// for
	// while
	// do while
	
	int i, j, k;

	// if without else
	if (i && ! j)
	{
		k = 1;
	}

	// if with else
	if (i > 2 || j < 17)
	{
		k = 23;
	}
	else
	{
		k = 444;
	}

	// for loop
	for (i = 4; i < 10; ++i)
	{
		++k;
	}

	for (int y = 0, x = 0; x < 10; x++, y--)
	{
		i += 10;
		j += y + x;
	}

	// while loop
	i = 0; j = 0;
	while (i < 10) j += i++;

	// do while loop
	i = 0; j = 0;
	do
	{
		j += ++i;
	}
	while (i < 10);
}

int main (int argc, char ** argv)
{
	int i;
	test1 ();
	test2 ();
	f1 ();
	f2 ();
	i = f3 (1, 2, 3.0);
	printf ("f3 %d\n", i);
	i = * f4 (&argc);
	printf ("f4 %d\n", i);

	return 0;
}
