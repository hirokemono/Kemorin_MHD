#
#
#

INT32DIR = $$(CORE_SRCDIR)/32bit_Integer
SOURCES = $(shell ls *.f90)

MOD_INT32 = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_INT32'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INT32DIR = $(INT32DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_INT32)'         >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(INT32DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
