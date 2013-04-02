SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = isp
CUDALIBDIR = /usr/local/cuda/lib64
CUDALIB = cudart

.SUFFIXES: .f .o .out

clean:
	rm -f *.out
	rm -f *~

.f.out:
	$(FC) $(FFLAGS) $< -L$(LIBDIR) -l$(LIB) -L$(CUDALIBDIR) -l$(CUDALIB) \
	-o $@
