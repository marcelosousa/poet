
R:=$(PWD)
#R=.

INC:=-I $R/include -I $R/src
INC+=-I $R/include/bits64/
INC+=-I $R/include/x86_64-linux-gnu/ 
INC+=-I $R/include/arch/x86_64/
CPPFLAGS:=-nostdinc -D__KLIBC__ $(INC)
CFLAGS:=-Wall -g
CC:=gcc
CILLY:=cilly --merge --verbose

SRCS:=$(wildcard $R/src/*.c)
SRCS+=$(wildcard $R/src/ctype/*.c)
SRCS+=$(wildcard $R/src/inet/*.c)
SRCS+=$(wildcard $R/src/stdio/*.c)
SRCS+=$(wildcard $R/src/userdb/*.c)
SRCS+=$(wildcard $R/src/zlib/*.c)
OBJS:=$(SRCS:.c=.o)
XOBJS:=$(SRCS:.c=.xo)
DEPS:=$(patsubst %.o,%.d,$(OBJS) $(MOBJS))

all : libc.a libc.o libc.c

libc.a : $(OBJS)
	ar cr $@ $^

libc.o : $(OBJS)
	ld -i $^ -o $@ # incremental link

libc.c : $(XOBJS)
	-$(CILLY) $^ --out=$@ -o wont-compileeee
	-@echo
	-@echo
	-@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-@echo "The previous cilly execution is EXPECTED to have linking errors."
	-@echo "That's OK, as long as it has generated the file 'libc.c',"
	-@echo "which should have around 28K lines :)"
	-@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-@echo
	-@echo
	ls -l libc.c
	wc -l libc.c
	-@echo

tags :
	ctags -R include/ src/ Makefile inc.mk

clean :
	rm -f $(OBJS)
	rm -f $(XOBJS)
	rm -f libc.[oa]
	rm -f wont-compile* ___extra_files
	(test -f libc.c && mv libc.c libc.c.old) || true

distclean : clean
	rm -f $(DEPS)
	rm -f libc.c.old

vars :
	@echo $(OBJS)

%.d : %.c
	@echo "DEP $<"
	@set -e; gcc -MM -MT $*.o $(CPPFLAGS) $< | sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' > $@;

%.xo : %.c
	$(CILLY) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

-include $(DEPS)

