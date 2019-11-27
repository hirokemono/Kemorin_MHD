#
#
#

SOLVER_11_DIR = $$(CRS_SOLVER_DIR)/scalar
SOURCES = $(shell ls *.f90)
MOD_SOLVER_11 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_11_DIR =  $(SOLVER_11_DIR)'       >> $(MAKENAME)

mod_list:
	@echo MOD_SOLVER_11= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_11) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_11_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
