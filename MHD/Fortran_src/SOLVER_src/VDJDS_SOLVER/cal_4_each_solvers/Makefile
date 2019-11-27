#
#
#

SOLVER_EACH_DIR =     $$(DJDS_SOLVER_DIR)/cal_4_each_solvers
SOURCES = $(shell ls *.f90)
MOD_SOLVE_EACH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_EACH_DIR = $(SOLVER_EACH_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_EACH= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_EACH) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_EACH_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
