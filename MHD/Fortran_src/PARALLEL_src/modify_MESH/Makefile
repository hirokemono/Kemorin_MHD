#
#
#

MODIFY_PARA_MESHDIR =    $$(PARA_SRCDIR)/modify_MESH

LIB_MODIFY_PARA_MESH =      -lpara_modify_mesh
LIB_MODIFY_PARA_MESH_FILE = libmodify_mesh.a

SRC_MODIFY_PARA_MESH = $(shell ls *.f90 *.F90)
MOD_MODIFY_PARA_MESH = $(addsuffix .o,$(basename $(SRC_MODIFY_PARA_MESH)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MODIFY_PARA_MESHDIR = $(MODIFY_PARA_MESHDIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_MODIFY_PARA_MESH =      $(LIB_MODIFY_PARA_MESH)'      >> $(MAKENAME)
	@echo  'LIB_MODIFY_PARA_MESH_FILE = $(LIB_MODIFY_PARA_MESH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MODIFY_PARA_MESH_FILE): $$(MOD_MODIFY_PARA_MESH)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_MODIFY_PARA_MESH)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_MODIFY_PARA_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_MODIFY_PARA_MESH) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MODIFY_PARA_MESHDIR)' $(SRC_MODIFY_PARA_MESH)
