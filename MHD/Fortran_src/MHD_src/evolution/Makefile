#
#
#

MHD_EVO_DIR = $$(MHD_SRCDIR)/evolution
SRC_EVOLVE = $(shell ls *.f90)
MOD_EVOLVE = $(addsuffix .o,$(basename $(SRC_EVOLVE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_EVO_DIR = $(MHD_EVO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_EVOLVE)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_EVOLVE=  \\ >> $(MAKENAME)
	@echo  $(MOD_EVOLVE) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_EVO_DIR)' $(SRC_EVOLVE)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
