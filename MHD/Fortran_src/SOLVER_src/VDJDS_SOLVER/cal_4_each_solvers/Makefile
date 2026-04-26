#
#
#

SOLVER_EACH_DIR =     $$(DJDS_SOLVER_DIR)/cal_4_each_solvers
SOURCES = $(shell ls *.f90)

MOD_SOLVE_EACH = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_EACH'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_EACH_DIR = $(SOLVER_EACH_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\  >> $(MAKENAME)
	@echo '$(MOD_SOLVE_EACH)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_EACH_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
