#
#
#

DCL_DIR =    $$(EXT_DIR)/DCL_custum
ISPACK_DIR = $$(DCL_DIR)/ispack-0.931/ftpack/src

LIB_ISPACK =       -lispack_fft
LIB_ISPACK_FILE = libispack_fft.a

MOD_ISPACK = \
ftdmsg.o \
ftr.o \
ftz.o \
ftct.o \
ftst.o \
ftcm.o \
ftsm.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DCL_DIR =    $(DCL_DIR)'    >> $(MAKENAME)
	@echo 'ISPACK_DIR = $(ISPACK_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_ISPACK =      $(LIB_ISPACK)'      >> $(MAKENAME)
	@echo  'LIB_ISPACK_FILE = $(LIB_ISPACK_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_ISPACK_FILE): $$(MOD_ISPACK)'>> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ISPACK)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ISPACK=  \\ >> $(MAKENAME)
	@echo  $(MOD_ISPACK) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@echo 'ftdmsg.o: $$(ISPACK_DIR)/ftdmsg.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftr.o: $$(ISPACK_DIR)/ftr.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftz.o: $$(ISPACK_DIR)/ftz.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftct.o: $$(ISPACK_DIR)/ftct.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftst.o: $$(ISPACK_DIR)/ftst.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftcm.o: $$(ISPACK_DIR)/ftcm.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo 'ftsm.o: $$(ISPACK_DIR)/ftsm.f' >> $(MAKENAME)
	@echo '	' '$$(F90) -c $$(F90OPTFLAGS) $$<' >> $(MAKENAME)
	@echo   >> $(MAKENAME)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
