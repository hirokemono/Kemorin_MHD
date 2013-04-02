#
#       Makefile for ispack
#

include Mkinclude

all:
	@for i in $(DIRS) ; do \
	  cd $$i/src ; \
	  echo "Making $$i/src" ; \
	  $(MAKE) ; \
	  cd ../.. ; \
	done
	cd .fftj;$(MAKE) $(SSE);cd ..
	$(AR) $(ARFLAGS) $(STATICLIB) */src/*.o .fftj/*/*.o
	$(RANLIB) $(STATICLIB)

clean:
	@for i in $(DIRS) ; do \
	  cd $$i/src ; \
	  echo "Cleaning $$i/src" ; \
	  $(MAKE) clean ; \
	  cd ../.. ; \
	done
	cd .fftj; $(MAKE) clean	

veryclean: clean
	echo "Cleaning lib"
	$(RM) $(STATICLIB)
	$(RM) *~
	$(RM) */*~
	$(RM) */*/*~
	$(RM) */*/*.dat
	$(RM) */*/*.out	
	$(RM) */doc/*.dvi
	$(RM) */doc/*.log	
	$(RM) */doc/*.aux
	cd .fftj; $(MAKE) veryclean
