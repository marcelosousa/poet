
PLC_R=~/x/devel/poet/libc
PLC_I=-nostdinc -D__KLIBC__
#PLC_I=-D__KLIBC__
PLC_I+=-I $(PLC_R)/include
PLC_I+=-I $(PLC_R)/include/bits64/
PLC_I+=-I $(PLC_R)/include/x86_64-linux-gnu/ 
PLC_I+=-I $(PLC_R)/include/arch/x86_64/

PLC_CPPFLAGS=$(PLC_I)
PLC_LDLIBS_A=$(PLC_R)/libc.a
PLC_LDLIBS_C=$(PLC_R)/libc.c
#PLC_LDFLAGS:=
#PLC_CFLAGS:=
#PLC_CC:=

