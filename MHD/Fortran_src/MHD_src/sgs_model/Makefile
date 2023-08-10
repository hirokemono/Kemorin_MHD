#
#
#

MHD_SGS_DIR = $$(MHD_SRCDIR)/sgs_model
SOURCES = $(shell ls *.f90)
MOD_SGS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SGS_DIR = $(MHD_SGS_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SGS)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SGS=  \\ >> $(MAKENAME)
	@echo $(MOD_SGS) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_SGS_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
