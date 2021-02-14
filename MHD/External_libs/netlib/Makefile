#
#
#

NETLIB_DIR =      $$(EXT_DIR)/netlib
NETLIB_SRC_DIR =  $$(NETLIB_DIR)/src

LIB_NETLIB =      -lnetlib
LIB_NETLIB_FILE = libnetlib.a

MOD_NETLIB = \
dsort_with_double.o \
dsort_with_int.o \
dsort_with_int8.o \
isort_with_int.o \
isort_with_int8.o \
i8sort_with_int.o \
i8sort_with_int8.o

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

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_NETLIB_FILE): $$(MOD_NETLIB)'>> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_NETLIB)' >> $(MAKENAME)

mod_list:
	@echo  MOD_NETLIB=  \\ >> $(MAKENAME)
	@echo  $(MOD_NETLIB) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@echo 'dsort_with_double.o: $$(NETLIB_SRC_DIR)/dsort_with_double.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'dsort_with_int.o: $$(NETLIB_SRC_DIR)/dsort_with_int.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'dsort_with_int8.o: $$(NETLIB_SRC_DIR)/dsort_with_int8.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'isort_with_int.o: $$(NETLIB_SRC_DIR)/isort_with_int.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'isort_with_int8.o: $$(NETLIB_SRC_DIR)/isort_with_int8.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'i8sort_with_int.o: $$(NETLIB_SRC_DIR)/i8sort_with_int.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'i8sort_with_int8.o: $$(NETLIB_SRC_DIR)/i8sort_with_int8.f90' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
