#
#
#

SOLVER_MATVEC_DIR =     $$(DJDS_SOLVER_DIR)/cal_matvecs
SOURCES = $(shell ls *.f90)
MOD_SOLVE_MATVEC = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_MATVEC_DIR = $(SOLVER_MATVEC_DIR)' >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVE_MATVEC= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVE_MATVEC) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_MATVEC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
