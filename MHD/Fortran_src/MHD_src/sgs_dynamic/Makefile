#
#
#

MHD_SGS_DYNAMIC_DIR = $$(MHD_SRCDIR)/sgs_dynamic
SOURCES = $(shell ls *.f90)
MOD_SGS_DYNAMIC = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SGS_DYNAMIC_DIR = $(MHD_SGS_DYNAMIC_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SGS_DYNAMIC)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SGS_DYNAMIC=  \\ >> $(MAKENAME)
	@echo $(MOD_SGS_DYNAMIC) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_SGS_DYNAMIC_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
