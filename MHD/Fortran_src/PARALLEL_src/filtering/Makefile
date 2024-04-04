#
#
#

FILTER_DIR = $$(PARA_SRCDIR)/filtering
SOURCES = $(shell ls *.f90)
MOD_FILTERING = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_DIR = $(FILTER_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FILTERING)' >> $(MAKENAME)


mod_list:
	@echo MOD_FILTERING=  \\ >> $(MAKENAME)
	@echo $(MOD_FILTERING) >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FILTER_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
