#
#
#

MHD_RHS_DIR = $$(MHD_SRCDIR)/fem_mhd_integration
SOURCES = $(shell ls *.f90)

MOD_RHS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_RHS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_RHS_DIR = $(MHD_RHS_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_RHS)'     >> $(MAKENAME)
	@echo  '#'>> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_RHS_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
