#
#
#

SOLVER_NN_DIR = $$(CRS_SOLVER_DIR)/blockNN
SOURCES = $(shell ls *.f90)
MOD_SOLVER_NN = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_NN_DIR =  $(SOLVER_NN_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_NN= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_NN) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_NN_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
