#
#
#

FEM_INT_DIR = $$(PARA_SRCDIR)/FEM_base_integration
SOURCES = $(shell ls *.f90)

MOD_FEM_INT = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FEM_INT'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_INT_DIR = $(FEM_INT_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_FEM_INT)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FEM_INT_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
