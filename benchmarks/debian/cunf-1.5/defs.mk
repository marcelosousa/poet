
# Copyright (C) 2010, 2011  Cesar Rodriguez <cesar.rodriguez@lsv.ens-cachan.fr>
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

R=$(PWD)
#R=.

include ../../../libc/inc.mk

# traditional variables
CFLAGS:=-Wall -g -fno-builtin
LDFLAGS:=

#CPPFLAGS:=-I $R/include/
#LDLIBS:=
CPPFLAGS:=-I $R/include/ $(PLC_CPPFLAGS)
LDLIBS:=$(PLC_LDLIBS_C)
#LDLIBS:=$(PLC_LDLIBS_A)


# object file targets
SRCS:=$(wildcard $R/src/*/*.c)
SRCS+=$(filter-out %/main.c, $(wildcard $R/src/*.c))

# object files containing a main() function
MSRCS:=$(wildcard $R/src/main.c)
#MSRCS+=$(wildcard $R/test/*.c) // we only want cunf

# compilation targets
OBJS:=$(patsubst %.c,%.o,$(SRCS))
MOBJS:=$(patsubst %.c,%.o,$(MSRCS))
TARGETS:=$(patsubst %.o,%,$(MOBJS))

# dependency files
DEPS:=$(patsubst %.o,%.d,$(OBJS) $(MOBJS))

# define the toolchain
CROSS:=

LD:=$(CROSS)ld
#CC:=$(CROSS)gcc
CC:=cilly --merge --out=out.c --dormunreach
CXX:=$(CROSS)g++
CPP:=$(CROSS)cpp

%.d : %.c
	@echo "DEP $<"
	@set -e; $(CC) -MM -MT $*.o $(CPPFLAGS) $< | \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' > $@;

.c.o:
	@echo "CC  $<"
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

%.pdf : %.dot
	@echo "DOT $<"
	@dot -T pdf < $< > $@

%.jpg : %.dot
	@echo "DOT $<"
	@dot -T jpg < $< > $@

%.dot : %.ll_net
	@echo "N2D $<"
	@test/net2dot $< > $@
	@#tools/pep2dot.py < $< > $@

%.ll_net : %.cuf
	@echo "C2P $<"
	@tools/cuf2pep.py < $< > $@

%.unf.cuf : %.ll_net
	@echo "UNF $<"
	@src/main $<

%.mp.mp : %.unf.cuf
	@echo "MER $<"
	@tools/merger.py < $< > $@

%.unf.mci : %.ll_net
	@echo "MLE $<"
	@mole $< -m $@

%.info : %.ll_net
	@test/info $< | tools/distrib.py

%.ll_net : %.xml
	@echo "P2P $<"
	@tools/pnml2pep.pl < $< > $@

%.ll_net : %.gml
	@echo "G2P $<"
	@tools/gml2pep.py < $< > $@

%.r : %.dot
	@echo "RS  $<"
	@tools/rs.pl $< > $@
