#
#
#

SOLVER_PRECOND_DIR =     $$(DJDS_SOLVER_DIR)/precondition
SOURCES = $(shell ls *.f90)

MOD_SOLVE_PRECONDS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_PRECONDS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_PRECOND_DIR = $(SOLVER_PRECOND_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\  >> $(MAKENAME)
	@echo '$(MOD_SOLVE_PRECONDS)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_PRECOND_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
