#
#
#

DJDS_ORDER_DIR = $$(PARA_SRCDIR)/ORDERING_4_DJDS
SRC_SOLVER_SET_ORDER = $(shell ls *.f90)
MOD_SOLVER_SET_ORDER = $(addsuffix .o,$(basename $(SRC_SOLVER_SET_ORDER)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DJDS_ORDER_DIR = $(DJDS_ORDER_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SOLVER_SET_ORDER)' >> $(MAKENAME)


mod_list:
	@echo MOD_SOLVER_SET_ORDER= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_SET_ORDER) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(DJDS_ORDER_DIR)' $(SRC_SOLVER_SET_ORDER)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
