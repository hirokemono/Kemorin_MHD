#
#
#

MHD_AMG_DIR = $$(MHD_SRCDIR)/MG_for_MHD
SOURCES = $(shell ls *.f90)
MOD_MG_4_MHD = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_AMG_DIR = $(MHD_AMG_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MG_4_MHD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_MG_4_MHD=  \\ >> $(MAKENAME)
	@echo $(MOD_MG_4_MHD)   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_AMG_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
