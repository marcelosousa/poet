
int *i,a;

struct test2
{
  struct t { int j;};
  struct t k;
  int l;
};

typedef struct test2 test2_t;

static int x;
static int *x;
char *argv[];
int (*ptar)[ 10 ];
int (*fp)();
int *(*list[ MAX ])();
char *table[ 10 ][ 20 ];

int func( char *, int );

const int *ptr1;
int *const ptr2;
extern char *const (*goop( char *b ))( int, long );

register short y = 0;
char ****q[ 30 ];
char **(**q)[ 30 ];
extern int (x)[];
long (*a[])( char, char );
int *(*(*(*b)())[10])();
char *strprt( char (*)( int ), unsigned char );
int (*const ab[])( unsigned int );

