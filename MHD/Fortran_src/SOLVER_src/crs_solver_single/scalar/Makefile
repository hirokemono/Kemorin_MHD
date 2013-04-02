#
#
#

SOLVER_11S_DIR = $$(CRS_SOLVER_S_DIR)/scalar
SRC_SOLVER_11_S = $(shell ls *.f90)
MOD_SOLVER_11_S = $(addsuffix .o,$(basename $(SRC_SOLVER_11_S)) )

MOD_SOLVER_11_S= \
\
solver_single.o \
\
solver_BiCGSTAB_single.o \
solver_CG_single.o \
solver_GMRES_single.o \
solver_GPBiCG_single.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_11S_DIR =  $(SOLVER_11S_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_11_S= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_11_S) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_11S_DIR)' $(SRC_SOLVER_11_S)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
