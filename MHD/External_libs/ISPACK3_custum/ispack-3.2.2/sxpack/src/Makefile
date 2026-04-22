OBJS = $(addsuffix .o,$(basename $(wildcard ??????.f90 *-$(SSE4).* *-$(SSE8).*)))
include ../../Mkinclude.sub
.SUFFIXES :
.SUFFIXES : .o .f90 .s
.f90.o :
	$(FC) $(FFLAGS) -c $<
.s.o :
	$(AS) $< -o $@
