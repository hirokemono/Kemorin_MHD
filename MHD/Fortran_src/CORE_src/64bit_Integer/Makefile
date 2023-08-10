#
#
#

INT64DIR = $$(CORE_SRCDIR)/64bit_Integer
SOURCES = $(shell ls *.f90)
MOD_INT64 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INT64DIR = $(INT64DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_INT64)' >> $(MAKENAME)


mod_list:
	@echo  MOD_INT64=  \\ >> $(MAKENAME)
	@echo  $(MOD_INT64) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(INT64DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
