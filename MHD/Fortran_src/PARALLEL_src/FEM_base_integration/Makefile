#
#
#

FEM_INT_DIR = $$(PARA_SRCDIR)/FEM_base_integration
SRC_FEM_INT = $(shell ls *.f90)
MOD_FEM_INT = $(addsuffix .o,$(basename $(SRC_FEM_INT)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_INT_DIR = $(FEM_INT_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FEM_INT)' >> $(MAKENAME)


mod_list:
	@echo MOD_FEM_INT=  \\ >> $(MAKENAME)
	@echo $(MOD_FEM_INT) >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FEM_INT_DIR)' $(SRC_FEM_INT)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
