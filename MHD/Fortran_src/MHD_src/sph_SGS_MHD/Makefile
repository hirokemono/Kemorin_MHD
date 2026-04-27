#
#
#

SGS_MHD_SPH_DIR = $$(MHD_SRCDIR)/sph_SGS_MHD
SOURCES = $(shell ls *.f90)

MOD_SPH_SGS_MHD = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_SPH_SGS_MHD'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SGS_MHD_SPH_DIR = $(SGS_MHD_SPH_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_SPH_SGS_MHD)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SGS_MHD_SPH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
