#
#
#

SOLVER_VECCAL_DIR =     $$(DJDS_SOLVER_DIR)/cal_vectors
SOURCES = $(shell ls *.f90)
MOD_SOLVE_VECCALS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_VECCAL_DIR = $(SOLVER_VECCAL_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_VECCALS= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_VECCALS) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_VECCAL_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
