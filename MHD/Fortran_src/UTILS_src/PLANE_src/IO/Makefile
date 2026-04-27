#
#
#

PLANE_IO_DIR = $$(PLANE_SRCDIR)/IO
SOURCES = $(shell ls *.f90)

MOD_PLANE_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PLANE_IO'
UPPER_MODULES=

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_IO_DIR = $(PLANE_IO_DIR)' >> $(MAKENAME)

dir_list:

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PLANE_IO)'      >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PLANE_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
