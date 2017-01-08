
POET_R=~/x/devel/poet

# define the following defs to use our klibc
POET_I=-nostdinc -D__KLIBC__
POET_I+=-I $(POET_R)/libc/include
POET_I+=-I $(POET_R)/libc/include/bits64/
POET_I+=-I $(POET_R)/libc/include/x86_64-linux-gnu/ 
POET_I+=-I $(POET_R)/libc/include/arch/x86_64/

POET_CPPFLAGS=$(POET_I)
POET_LDLIBS_A=$(POET_R)/libc/libc.a
POET_LDLIBS_C=$(POET_R)/libc/libc.c
POET_CFLAGS:=-fno-builtin
#POET_LDFLAGS:=

#POET_CC:=$(POET_R)/tools/cil-1.7.3/bin/cilly --merge --out=full.c --dormunreach
POET_CC:=$(POET_R)/tools/cil-1.7.3/bin/cilly --merge --out=full.c --dopoet

