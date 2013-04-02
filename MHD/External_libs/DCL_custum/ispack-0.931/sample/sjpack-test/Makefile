SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = isp

clean:
	rm -f *.out
	rm -f *~
	
%-align.out: %-align.o %-align-sub.o
	$(FC) $(FFLAGS) $^ -L$(LIBDIR) -l$(LIB) -o $@
	
%.out: %.o
	$(FC) $(FFLAGS) $< -L$(LIBDIR) -l$(LIB) -o $@

