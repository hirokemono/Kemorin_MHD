#
#
#

SOLVER_ORDERING_DIR =     $$(DJDS_SOLVER_DIR)/orderings
SOURCES = $(shell ls *.f90)
MOD_SOLVE_ORDERINGS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_ORDERING_DIR = $(SOLVER_ORDERING_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_ORDERINGS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_ORDERINGS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_ORDERING_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
