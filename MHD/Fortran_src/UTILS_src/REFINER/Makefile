#
#
#

REFINER_DIR =     $$(UTILS_SRCDIR)/REFINER

LIB_REFINER =       -lrefine_kemo
LIB_REFINER_FILE = librefine_kemo.a

SRC_REFINE = $(shell ls *.f90)
MOD_REFINE = $(addsuffix .o,$(basename $(SRC_REFINE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'REFINER_DIR = $(REFINER_DIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_REFINER =      $(LIB_REFINER)'      >> $(MAKENAME)
	@echo  'LIB_REFINER_FILE = $(LIB_REFINER_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_REFINE)' \
	>> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_REFINER_FILE): $$(MOD_REFINE)' >> $(MAKENAME)

mod_list:
	@echo  MOD_REFINE=  \\ >> $(MAKENAME)
	@echo  $(MOD_REFINE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(REFINER_DIR)' $(SRC_REFINE)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
