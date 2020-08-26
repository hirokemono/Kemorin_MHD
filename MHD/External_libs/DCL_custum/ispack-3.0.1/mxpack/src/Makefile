OBJS = $(addsuffix .o,$(basename $(wildcard ??????.c ??????.f90 *-$(SSE4).* *-$(SSE8).* *-$(CPU).f90)))
include ../../Mkinclude.sub
.SUFFIXES :
.SUFFIXES : .o .f90 .c
.f90.o :
	$(FC) $(FFLAGS) -c $<
.c.o :
	$(CC) $(CFLAGS) -c $< 
