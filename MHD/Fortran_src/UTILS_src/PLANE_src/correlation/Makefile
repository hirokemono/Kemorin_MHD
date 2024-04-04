#
#
#

CORRELATE_PLANE_DIR = $$(PLANE_SRCDIR)/correlation
SOURCES = $(shell ls *.f90)
MOD_CORRELATE_PLANE = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_CORRELATE_PLANE)' >> $(MAKENAME)

mod_list:
	@echo  MOD_CORRELATE_PLANE=  \\ >> $(MAKENAME)
	@echo  $(MOD_CORRELATE_PLANE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(CORRELATE_PLANE_DIR)' $(SOURCES)
