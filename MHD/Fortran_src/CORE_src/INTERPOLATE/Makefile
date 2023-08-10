#
#
#

INTERPOLATE_DIR = $$(CORE_SRCDIR)/INTERPOLATE
SOURCES = $(shell ls *.f90)
MOD_ITP = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INTERPOLATE_DIR = $(INTERPOLATE_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ITP)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ITP=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(INTERPOLATE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
