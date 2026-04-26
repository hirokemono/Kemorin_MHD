#
#
#

SET_PRECOND_DIR =     $$(DJDS_SOLVER_DIR)/set_precondition
SOURCES = $(shell ls *.f90)

MOD_SOLVE_SET_PRECONDS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_SET_PRECONDS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SET_PRECOND_DIR = $(SET_PRECOND_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\      >> $(MAKENAME)
	@echo '$(MOD_SOLVE_SET_PRECONDS)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SET_PRECOND_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
