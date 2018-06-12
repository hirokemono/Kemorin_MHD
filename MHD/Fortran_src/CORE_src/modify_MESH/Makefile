#
#
#

MODIFY_MESHDIR =    $$(CORE_SRCDIR)/modify_MESH

LIB_MODIFY_MESH =     -lmodify_mesh
LIB_MODIFY_MESH_FILE = libmodify_mesh.a

SRC_MODIFY_MESH = $(shell ls *.f90 *.F90)
MOD_MODIFY_MESH = $(addsuffix .o,$(basename $(SRC_MODIFY_MESH)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MODIFY_MESHDIR = $(MODIFY_MESHDIR)' >> $(MAKENAME)

lib_name:
	@echo  'LIB_MODIFY_MESH =      $(LIB_MODIFY_MESH)'      >> $(MAKENAME)
	@echo  'LIB_MODIFY_MESH_FILE = $(LIB_MODIFY_MESH_FILE)' >> $(MAKENAME)
	@echo   >> $(MAKENAME)

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:
	@echo '' >> $(MAKENAME)
	@echo  '$$(LIB_MODIFY_MESH_FILE): $$(MOD_MODIFY_MESH)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' \
	'$$(MOD_MODIFY_MESH)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_MODIFY_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_MODIFY_MESH) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MODIFY_MESHDIR)' $(SRC_MODIFY_MESH)
