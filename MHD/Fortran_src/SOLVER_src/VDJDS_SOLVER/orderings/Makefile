#
#
#

SOLVER_ORDERING_DIR =     $$(DJDS_SOLVER_DIR)/orderings
SRC_SOLVE_ORDERINGS = $(shell ls *.f90)
MOD_SOLVE_ORDERINGS = $(addsuffix .o,$(basename $(SRC_SOLVE_ORDERINGS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_ORDERING_DIR = $(SOLVER_ORDERING_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_ORDERINGS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_ORDERINGS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_ORDERING_DIR)' $(SRC_SOLVE_ORDERINGS)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
