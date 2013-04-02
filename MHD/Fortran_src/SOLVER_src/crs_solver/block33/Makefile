#
#
#

SOLVER_33_DIR = $$(CRS_SOLVER_DIR)/block33
SRC_SOLVER_33 = $(shell ls *.f90)
MOD_SOLVER_33 = $(addsuffix .o,$(basename $(SRC_SOLVER_33)) )

MOD_SOLVER_33= \
\
solver33.o \
\
solver_CG_3.o \
solver_BiCGSTAB_3.o \
solver_GPBiCG_3.o \
solver_GMRES_3.o \
\
solver_BLCG_3.o \
solver_BLBiCGSTAB_3.o \
solver_BLGPBiCG_3.o \
\
crs_matrix_calcs_33.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_33_DIR =  $(SOLVER_33_DIR)'       >> $(MAKENAME)


mod_list:
	@echo MOD_SOLVER_33= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_33) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_33_DIR)' $(SRC_SOLVER_33)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
