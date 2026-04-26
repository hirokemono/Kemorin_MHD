#
#
#

SOLVER_ORDERING_DIR =     $$(DJDS_SOLVER_DIR)/orderings
SOURCES = $(shell ls *.f90)

MOD_SOLVE_ORDERINGS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVE_ORDERINGS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_ORDERING_DIR = $(SOLVER_ORDERING_DIR)' >> $(MAKENAME)

mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\   >> $(MAKENAME)
	@echo '$(MOD_SOLVE_ORDERINGS)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_ORDERING_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
