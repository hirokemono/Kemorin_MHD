#
#
#

SOLVER_VECCAL_DIR =     $$(DJDS_SOLVER_DIR)/cal_vectors
SOURCES = $(shell ls *.f90)

MOD_SOLVE_VECCALS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_VECCALS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_VECCAL_DIR = $(SOLVER_VECCAL_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\  >> $(MAKENAME)
	@echo '$(MOD_SOLVE_VECCALS)'  >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_VECCAL_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
