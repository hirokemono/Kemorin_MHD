#
#
#

MHD_F2003_DIR = $$(MHD_SRCDIR)/Fortran2003
SOURCES = $(shell ls *.f90)
MOD_MHD_F2003 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_F2003_DIR = $(MHD_F2003_DIR)'  >> $(MAKENAME)

lib_archve:
#	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MHD_F2003)' \
#	>> $(MAKENAME)

mod_list:
	@echo MOD_MHD_F2003=  \\ >> $(MAKENAME)
	@echo $(MOD_MHD_F2003) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_F2003_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
