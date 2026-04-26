#
#
#

SOLVER_MATVEC_DIR =     $$(DJDS_SOLVER_DIR)/cal_matvecs
SOURCES = $(shell ls *.f90)

MOD_SOLVE_MATVEC = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_MATVEC'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_MATVEC_DIR = $(SOLVER_MATVEC_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\  >> $(MAKENAME)
	@echo '$(MOD_SOLVE_MATVEC)'   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_MATVEC_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
