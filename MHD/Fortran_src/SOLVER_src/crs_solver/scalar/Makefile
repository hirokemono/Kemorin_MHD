#
#
#

SOLVER_11_DIR = $$(CRS_SOLVER_DIR)/scalar
SOURCES = $(shell ls *.f90)
MOD_SOLVER_11 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_11_DIR =  $(SOLVER_11_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_11= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_11) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_11_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
