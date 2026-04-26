#
#
#

MESH_TYPE_DIR = $$(CORE_SRCDIR)/MESH_type
SOURCES = $(shell ls *.f90)

MOD_MESH_T = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MESH_T'
UPPER_MODULES=


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TYPE_DIR = $(MESH_TYPE_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MESH_T)'        >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MESH_TYPE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
