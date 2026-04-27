#
#
#

MHD_AMG_DIR = $$(MHD_SRCDIR)/MG_for_MHD
SOURCES = $(shell ls *.f90)

MOD_MG_4_MHD = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MG_4_MHD'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_AMG_DIR = $(MHD_AMG_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MG_4_MHD)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_AMG_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
