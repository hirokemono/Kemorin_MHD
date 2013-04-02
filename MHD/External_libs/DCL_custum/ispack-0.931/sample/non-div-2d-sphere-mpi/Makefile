SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = isp

.SUFFIXES: .f .o .out

clean:
	rm -f *.out
	rm -f *.o	
	rm -f *~

.f.out:
	$(MPIFC) $(MPIFFLAGS) $< -L$(LIBDIR) -l$(LIB)  \
	-o $@
