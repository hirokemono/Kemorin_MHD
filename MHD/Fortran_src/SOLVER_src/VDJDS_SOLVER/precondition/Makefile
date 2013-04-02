#
#
#

SOLVER_PRECOND_DIR =     $$(DJDS_SOLVER_DIR)/precondition
SRC_SOLVE_PRECONDS = $(shell ls *.f90)
MOD_SOLVE_PRECONDS = $(addsuffix .o,$(basename $(SRC_SOLVE_PRECONDS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_PRECOND_DIR = $(SOLVER_PRECOND_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_PRECONDS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_PRECONDS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_PRECOND_DIR)' $(SRC_SOLVE_PRECONDS)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
