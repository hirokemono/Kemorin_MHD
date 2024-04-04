#
#
#

PLANE_COMMON_DIR = $$(PLANE_SRCDIR)/plane_core
SOURCES = $(shell ls *.f90)
MOD_PLANE_COMMON = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_PLANE_COMMON)' >> $(MAKENAME)

mod_list:
	@echo  MOD_PLANE_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_PLANE_COMMON) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PLANE_COMMON_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.a; \
	cd $(ISPACK_DIR); \
	S(GMAKE) \
	GMAKE='$(GMAKE)' \
	clean
