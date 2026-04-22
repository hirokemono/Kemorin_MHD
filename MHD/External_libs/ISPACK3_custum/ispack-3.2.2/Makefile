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
	$(AR) $(ARFLAGS) $(STATICLIB) */src/*.o
	$(RANLIB) $(STATICLIB)
	$(LD) $(LDFLAGS) -shared -o $(SHAREDLIB) */src/*.o 

clean:
	@for i in $(DIRS) ; do \
	  cd $$i/src ; \
	  echo "Cleaning $$i/src" ; \
	  $(MAKE) clean ; \
	  cd ../.. ; \
	done

veryclean: clean
	echo "Cleaning lib"
	$(RM) $(STATICLIB)
	$(RM) $(SHAREDLIB)	
	$(RM) *~
	$(RM) */*~
	$(RM) */*/*~
	$(RM) */*/*.dat
	$(RM) */*/*.out	
	$(RM) */doc/*.dvi
	$(RM) */doc/*.log	
	$(RM) */doc/*.aux
