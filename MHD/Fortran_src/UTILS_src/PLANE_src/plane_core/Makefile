#
#
#

PLANE_COMMON_DIR = $$(PLANE_SRCDIR)/plane_core
SOURCES = $(shell ls *.f90)

MOD_PLANE_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PLANE_COMMON'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_COMMON_DIR = $(PLANE_COMMON_DIR)'   >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_PLANE_COMMON)'  >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PLANE_COMMON_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.a; \
	cd $(ISPACK_DIR); \
	S(GMAKE) \
	GMAKE='$(GMAKE)' \
	clean
