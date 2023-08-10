#
#
#

FEM_INT_DIR = $$(PARA_SRCDIR)/FEM_base_integration
SOURCES = $(shell ls *.f90)
MOD_FEM_INT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FEM_INT_DIR = $(FEM_INT_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FEM_INT)' >> $(MAKENAME)


mod_list:
	@echo MOD_FEM_INT=  \\ >> $(MAKENAME)
	@echo $(MOD_FEM_INT) >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FEM_INT_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
