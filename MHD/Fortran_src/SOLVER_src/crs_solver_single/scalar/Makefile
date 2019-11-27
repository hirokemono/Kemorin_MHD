#
#
#

SOLVER_11S_DIR = $$(CRS_SOLVER_S_DIR)/scalar
SOURCES = $(shell ls *.f90)
MOD_SOLVER_11_S = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_11S_DIR =  $(SOLVER_11S_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_11_S= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_11_S) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_11S_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
