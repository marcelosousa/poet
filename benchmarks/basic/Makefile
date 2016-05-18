VERS=3.7
CC=clang-$(VERS)
CXX=clang++-$(VERS)

CPPFLAGS=
#CFLAGS=-Wall -O1
CFLAGS=
CXXFLAGS=-Wall -O1
LDLIBS=-lpthread

SRCS:=$(wildcard *.c)
BINS:=$(SRCS:.c=)
LLS:=$(SRCS:.c=.ll)

all: $(BINS)
all-ll : $(LLS)

clean :
	rm -f $(BINS) $(LLS)

%.ll : %.c
	$(CC) -S -emit-llvm $(CFLAGS) $< -o $@
%.bc : %.c
	$(CC) -c -emit-llvm $(CFLAGS) $< -o $@
%.ll : %.cc
	$(CC) -S -emit-llvm $(CXXFLAGS) $< -o $@
%.bc : %.cc
	$(CC) -c -emit-llvm $(CXXFLAGS) $< -o $@
%.s : %.bc
	llc-$(VERS) $< -o $@
