#
#
#

SET_PRECOND_DIR =     $$(DJDS_SOLVER_DIR)/set_precondition
SRC_SOLVE_SET_PRECONDS = $(shell ls *.f90)
MOD_SOLVE_SET_PRECONDS = $(addsuffix .o,$(basename $(SRC_SOLVE_SET_PRECONDS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SET_PRECOND_DIR = $(SET_PRECOND_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_SET_PRECONDS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_SET_PRECONDS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SET_PRECOND_DIR)' $(SRC_SOLVE_SET_PRECONDS)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
