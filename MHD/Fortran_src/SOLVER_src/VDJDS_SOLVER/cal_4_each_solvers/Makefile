#
#
#

SOLVER_EACH_DIR =     $$(DJDS_SOLVER_DIR)/cal_4_each_solvers
SRC_SOLVE_EACH = $(shell ls *.f90)
MOD_SOLVE_EACH = $(addsuffix .o,$(basename $(SRC_SOLVE_EACH)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_EACH_DIR = $(SOLVER_EACH_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_EACH= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_EACH) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_EACH_DIR)' $(SRC_SOLVE_EACH)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
