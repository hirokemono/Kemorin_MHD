#
#
#

CUBED_SPH_DIR =   $$(UTILS_SRCDIR)/cubed_sph

LIB_CUBED_SPH =       -lcubed_sph_kemo
LIB_CUBED_SPH_FILE = libcubed_sph_kemo.a

SRC_CUBED_SPH = $(shell ls *.f90)
MOD_CUBED_SPH = $(addsuffix .o,$(basename $(SRC_CUBED_SPH)) )

#
#  -------------------------------------------------------------------------
#

.f90.o:; $(F90) -c $(F90FLAGS) $<

dir_list:
	@echo 'CUBED_SPH_DIR = $(CUBED_SPH_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_CUBED_SPH =      $(LIB_CUBED_SPH)'      >> $(MAKENAME)
	@echo  'LIB_CUBED_SPH_FILE = $(LIB_CUBED_SPH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_CUBED_SPH_FILE): $$(MOD_CUBED_SPH)' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CUBED_SPH)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_CUBED_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBED_SPH)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CUBED_SPH_DIR)' $(SRC_CUBED_SPH)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

