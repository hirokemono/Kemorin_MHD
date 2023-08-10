#
#
#

SGS_MHD_SPH_DIR = $$(MHD_SRCDIR)/sph_SGS_MHD
SOURCES = $(shell ls *.f90)
MOD_SPH_SGS_MHD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SGS_MHD_SPH_DIR = $(SGS_MHD_SPH_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_SGS_MHD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SPH_SGS_MHD=  \\ >> $(MAKENAME)
	@echo $(MOD_SPH_SGS_MHD) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SGS_MHD_SPH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
