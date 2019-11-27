#
#
#

SOLVER_MAIN_DIR =     $$(DJDS_SOLVER_DIR)/main_4_solvers
SOURCES = $(shell ls *.f90)
MOD_SOLVER_MAINS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_MAIN_DIR = $(SOLVER_MAIN_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_MAINS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_MAINS) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_MAIN_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
