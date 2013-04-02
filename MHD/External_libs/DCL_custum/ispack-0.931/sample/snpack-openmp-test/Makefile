SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = isp

.SUFFIXES: .f .o .out

clean:
	rm -f *.out
	rm -f *~

.f.out:
	$(FC) $(FFLAGS) $< -L$(LIBDIR) -l$(LIB)  \
	-o $@
