#
#
#

MHD_RHS_BC_DIR = $$(MHD_SRCDIR)/rhs_bc_vector
SRC_RHS_BC = $(shell ls *.f90)
MOD_RHS_BC = $(addsuffix .o,$(basename $(SRC_RHS_BC)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_RHS_BC_DIR = $(MHD_RHS_BC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_RHS_BC)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_RHS_BC= \\ >> $(MAKENAME)
	@echo $(MOD_RHS_BC) >> $(MAKENAME)
	@echo '#'>> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_RHS_BC_DIR)' $(SRC_RHS_BC)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
