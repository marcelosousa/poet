
include ../../../libc/inc.mk

CFLAGS=-Wall -g
LDFLAGS:=

#CC=clang
#CC=gcc
CC=cilly --merge --out=out.c --dormunreach
CPPFLAGS:=
CPPFLAGS:=-I . $(PLC_CPPFLAGS)
#LDLIBS:=
LDLIBS:=$(PLC_LDLIBS_C)
#LDLIBS:=$(PLC_LDLIBS_A)

all : main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-./main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	gcc -Wall -c out.c
	readelf -s out.o | grep --color UND

out :
	gcc -o out out.c

clean :
	rm -f main *.o out.c
