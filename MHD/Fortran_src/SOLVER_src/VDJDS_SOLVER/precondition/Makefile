#
#
#

SOLVER_PRECOND_DIR =     $$(DJDS_SOLVER_DIR)/precondition
SOURCES = $(shell ls *.f90)
MOD_SOLVE_PRECONDS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_PRECOND_DIR = $(SOLVER_PRECOND_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_PRECONDS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_PRECONDS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_PRECOND_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
