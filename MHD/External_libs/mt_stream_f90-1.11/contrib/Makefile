#
# Makefile for test codes
#
# This direcotry does not contain mt19937ar.c [the original (non-multi-stream)
# Mersenne Twister of Takuji Nishimura and Makoto Matsumoto (2002/1/26)]. 
# You must first obtain mt19937ar.c from their Mersenne Twister web page.
#


#
# Use NTL/GF2[x] ?
#
USE_NTL := yes
#USE_NTL := no

#
# locate NTL/GF2[x]/C++ libralies.
#
NTL_LIBS :=  -L$(HOME)/lib -lntl -lgf2x -lstdc++
#NTL_LIBS :=

#
# Compiler settings
#
FC = ifort
CC = icc
FFLAGS = -O2 -D_DEBUG_
CFLAGS = -O2 -Dgenrand_int32=genrand_int32_C
LDFLAGS := -static


RM = rm -f

INCLUDES = -I../
ifeq ($(USE_NTL),yes)
LIBS = ../mt_stream_debug.o \
       ../jump_ahead_coeff/get_coeff.o \
       $(NTL_LIBS)
else
LIBS = ../mt_stream_debug.o \
       ../f_jump_ahead_coeff/f_get_coeff.o \
       ../f_jump_ahead_coeff/gf2xe.o
endif

PROGRAMS = test_FortMT_vs_ORIG.doubles test_FortMT_vs_ORIG.ints
all : $(PROGRAMS) test_real8.out test_integer.out

#
# remove main function 
#
mt19937ar_lib.c : mt19937ar.c
	head -173 $^ > $@

mt19937ar_lib.o : mt19937ar_lib.c
	$(CC) $(CFLAGS) -c $<

MSB_kind_defs.o : MSB_kind_defs.F90
	$(FC) $(FFLAGS) -c $<

test_FortMT_vs_ORIG.doubles.o : test_FortMT_vs_ORIG.doubles.F90 MSB_kind_defs.o
	$(FC) $(FFLAGS) -c $< $(INCLUDES)

test_FortMT_vs_ORIG.ints.o : test_FortMT_vs_ORIG.ints.F90 MSB_kind_defs.o
	$(FC) $(FFLAGS) -c $< $(INCLUDES)

test_FortMT_vs_ORIG.doubles : test_FortMT_vs_ORIG.doubles.o mt19937ar_lib.o
	$(FC) $(LDFLAGS) $^ $(LIBS) -o $@

test_FortMT_vs_ORIG.ints : test_FortMT_vs_ORIG.ints.o mt19937ar_lib.o
	$(FC) $(LDFLAGS) $^ $(LIBS) -o $@

test_real8.out : test_FortMT_vs_ORIG.doubles
	@echo
	@echo "# real(8) TEST"
	./test_FortMT_vs_ORIG.doubles > $@
	@tail $@
	@echo

test_integer.out : test_FortMT_vs_ORIG.ints
	@echo
	@echo "# integer TEST"
	./test_FortMT_vs_ORIG.ints > $@
	@tail $@
	@echo

clean :
	$(RM) $(PROGRAMS) *.o *.mod *~ *.bak \
	test_real8.out test_integer.out \
	mt19937ar_lib.o mt19937ar_lib.c
