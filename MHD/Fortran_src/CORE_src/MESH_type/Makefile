#
#
#

MESH_TYPE_DIR = $$(CORE_SRCDIR)/MESH_type
SOURCES = $(shell ls *.f90)
MOD_MESH_T = $(addsuffix .o,$(basename $(SOURCES)) )


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TYPE_DIR = $(MESH_TYPE_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH_T)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MESH_T=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH_T) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MESH_TYPE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
