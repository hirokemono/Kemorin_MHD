SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = ispack3

clean:
	rm -f *.out
	rm -f *~

%.out: %.f90
	$(FC) $(FFLAGS) $< -L$(LIBDIR) -l$(LIB) -o $@

