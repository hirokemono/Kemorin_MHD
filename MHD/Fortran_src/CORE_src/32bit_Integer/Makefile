#
#
#

INT32DIR = $$(CORE_SRCDIR)/32bit_Integer
SOURCES = $(shell ls *.f90)
MOD_INT32 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INT32DIR = $(INT32DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_INT32)' >> $(MAKENAME)


mod_list:
	@echo  MOD_INT32=  \\ >> $(MAKENAME)
	@echo  $(MOD_INT32) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(INT32DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
