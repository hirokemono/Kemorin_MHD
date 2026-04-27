#
#
#

CORRELATE_PLANE_DIR = $$(PLANE_SRCDIR)/correlation
SOURCES = $(shell ls *.f90)

MOD_CORRELATE_PLANE = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_CORRELATE_PLANE'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CORRELATE_PLANE_DIR = $(CORRELATE_PLANE_DIR)'  >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\   >> $(MAKENAME)
	@echo  '$(MOD_CORRELATE_PLANE)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(CORRELATE_PLANE_DIR)' $(SOURCES)
