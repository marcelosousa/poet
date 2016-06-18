
include ../../../libc/inc.mk

CFLAGS=-Wall -g -fno-builtin
LDFLAGS:=

### (1) Compiling using system headers and glibc
#CC=clang
#CPPFLAGS =
#LDLIBS = -pthread

### (2) Compiling using the headers of our klibc and a binary copy of it (libc.a)
#CC=clang
#CPPFLAGS = $(PLC_CPPFLAGS)
#LDLIBS = $(PLC_LDLIBS_A) -pthread

### (3) Compiling using the headers of our klibc and a source copy of it (libc.c)
#CC = cilly --merge --out=full.c --dormunreach
#CPPFLAGS = $(PLC_CPPFLAGS)
#LDLIBS = $(PLC_LDLIBS_C) -pthread

### (4) Compiling using the headers of our klibc, source copy of it (libc.c), and full inlining
CC = cilly --merge --out=full.c --dopoet
CPPFLAGS = $(PLC_CPPFLAGS)
LDLIBS = $(PLC_LDLIBS_C) -pthread

all : main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-./main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-gcc -Wall -c full.c
	-readelf -s full.o | grep --color UND

full : main

clean :
	rm -f main *.o full.c
