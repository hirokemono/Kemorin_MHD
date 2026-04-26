#
#
#

MODIFY_PARA_MESHDIR =    $$(PARA_SRCDIR)/modify_MESH

LIB_MODIFY_PARA_MESH =      -lpara_modify_mesh
LIB_MODIFY_PARA_MESH_FILE = libmodify_mesh.a

SOURCES = $(shell ls *.f90 *.F90)
MOD_MODIFY_PARA_MESH = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MODIFY_PARA_MESH'
UPPER_MODULES=

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MODIFY_PARA_MESHDIR = $(MODIFY_PARA_MESHDIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_MODIFY_PARA_MESH =      $(LIB_MODIFY_PARA_MESH)'      >> $(MAKENAME)
	@echo  'LIB_MODIFY_PARA_MESH_FILE = $(LIB_MODIFY_PARA_MESH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MODIFY_PARA_MESH_FILE): $$('$(MODULE_LIST)')'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)
	>> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MODIFY_PARA_MESH)'     >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MODIFY_PARA_MESHDIR)' $(SOURCES)
