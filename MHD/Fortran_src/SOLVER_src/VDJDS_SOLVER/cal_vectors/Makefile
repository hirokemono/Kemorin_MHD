#
#
#

SOLVER_VECCAL_DIR =     $$(DJDS_SOLVER_DIR)/cal_vectors
SRC_SOLVE_VECCALS = $(shell ls *.f90)
MOD_SOLVE_VECCALS = $(addsuffix .o,$(basename $(SRC_SOLVE_VECCALS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_VECCAL_DIR = $(SOLVER_VECCAL_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_VECCALS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_VECCALS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_VECCAL_DIR)' $(SRC_SOLVE_VECCALS)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
