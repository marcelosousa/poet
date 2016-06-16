
include ../../../libc/inc.mk

CFLAGS=-Wall -g
LDFLAGS:=

#CC=clang
CC=gcc -std=gnu99
#CC=cilly --merge --out=full.c --dormunreach --dopoet
#CPPFLAGS:=
#CPPFLAGS:=-I . $(PLC_CPPFLAGS)
LDLIBS:=-pthread
#LDLIBS:=$(PLC_LDLIBS_C) -pthread
#LDLIBS:=$(PLC_LDLIBS_A) -pthread

all : main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	-./main
	@echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	gcc -Wall -c full.c
	readelf -s full.o | grep --color UND

full :
	gcc -o full full.c

clean :
	rm -f main *.o full.c
