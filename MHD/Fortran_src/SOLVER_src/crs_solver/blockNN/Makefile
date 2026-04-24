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
	@echo '$(MOD_CRS_SOLVER)+= $$(MOD_SOLVER_NN)' >> $(MAKENAME)
	@echo 'MOD_SOLVER_NN=  ' \\  >> $(MAKENAME)
	@echo '$(MOD_SOLVER_NN)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_NN_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
