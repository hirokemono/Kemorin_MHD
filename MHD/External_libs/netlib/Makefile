#
#
#

NETLIB_DIR =      $$(EXT_DIR)/netlib
NETLIB_SRC_DIR =  $$(NETLIB_DIR)/src

LIB_NETLIB =      -lnetlib
LIB_NETLIB_FILE = libnetlib.a

MOD_NETLIB = \
dsort.o \
isort.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'NETLIB_DIR =      $(NETLIB_DIR)'    >> $(MAKENAME)
	@echo 'NETLIB_SRC_DIR =  $(NETLIB_SRC_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_NETLIB =       $(LIB_NETLIB)'       >> $(MAKENAME)
	@echo  'LIB_NETLIB_FILE =  $(LIB_NETLIB_FILE)'  >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_NETLIB_FILE): $$(MOD_NETLIB)'>> $(MAKENAME)

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_NETLIB)' >> $(MAKENAME)

mod_list:
	@echo  MOD_NETLIB=  \\ >> $(MAKENAME)
	@echo  $(MOD_NETLIB) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@echo 'dsort.o: $$(NETLIB_SRC_DIR)/dsort.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'isort.o: $$(NETLIB_SRC_DIR)/isort.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
