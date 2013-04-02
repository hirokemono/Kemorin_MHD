#
#
#

SOLVER_NN_DIR = $$(CRS_SOLVER_DIR)/blockNN
SRC_SOLVER_NN = $(shell ls *.f90)
MOD_SOLVER_NN = $(addsuffix .o,$(basename $(SRC_SOLVER_NN)) )

MOD_SOLVER_NN= \
\
solverNN.o \
\
solver_CG_N.o \
solver_BiCGSTAB_N.o \
solver_GPBiCG_N.o

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
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_NN_DIR)' $(SRC_SOLVER_NN)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
