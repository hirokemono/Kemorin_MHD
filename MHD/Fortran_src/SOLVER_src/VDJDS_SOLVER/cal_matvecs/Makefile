#
#
#

SOLVER_MATVEC_DIR =     $$(DJDS_SOLVER_DIR)/cal_matvecs
SRC_SOLVE_MATVEC = $(shell ls *.f90)
MOD_SOLVE_MATVEC = $(addsuffix .o,$(basename $(SRC_SOLVE_MATVEC)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_MATVEC_DIR = $(SOLVER_MATVEC_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_MATVEC= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_MATVEC) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_MATVEC_DIR)' $(SRC_SOLVE_MATVEC)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
