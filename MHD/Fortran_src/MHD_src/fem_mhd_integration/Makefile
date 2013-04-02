#
#
#

MHD_RHS_DIR = $$(MHD_SRCDIR)/fem_mhd_integration
SRC_RHS = $(shell ls *.f90)
MOD_RHS = $(addsuffix .o,$(basename $(SRC_RHS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_RHS_DIR = $(MHD_RHS_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_RHS)'  \
	>> $(MAKENAME)

mod_list:
	@echo MOD_RHS=  \\ >> $(MAKENAME)
	@echo $(MOD_RHS) >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_RHS_DIR)' $(SRC_RHS)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
