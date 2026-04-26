#
#
#

SOLVER_33_DIR = $$(CRS_SOLVER_DIR)/block33
SOURCES = $(shell ls *.f90)

MOD_SOLVER_33 = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVER_33'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_33_DIR =  $(SOLVER_33_DIR)'       >> $(MAKENAME)


mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_SOLVER_33)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_33_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
