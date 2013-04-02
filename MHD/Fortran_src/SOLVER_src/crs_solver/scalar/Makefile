#
#
#

SOLVER_11_DIR = $$(CRS_SOLVER_DIR)/scalar
SRC_SOLVER_11 = $(shell ls *.f90)
MOD_SOLVER_11 = $(addsuffix .o,$(basename $(SRC_SOLVER_11)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_11_DIR =  $(SOLVER_11_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_11= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_11) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_11_DIR)' $(SRC_SOLVER_11)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
