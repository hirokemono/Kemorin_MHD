SHELL = /bin/sh

include ../../Mkinclude

LIBDIR = ../../
LIB = isp

.SUFFIXES: .f .o .out

clean:
	rm -f *.out
	rm -f *~

.f.out:
	$(DCLFRT) $(FFLAGS) $< -L$(LIBDIR) -l$(LIB) -llapack \
	-o $@
