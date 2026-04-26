#
#
#

CTL_DIR = $$(CORE_SRCDIR)/CONTROLS
SOURCES = $(shell ls *.f90 *.F90)

MOD_CTL = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_CTL'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CTL_DIR = $(CTL_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)
	>> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_CTL)'           >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(CTL_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
