#
#
#

PLANE_MODEL_DIR =  $$(MESHER_DIR)/plane_layer

TARGET_CUBMESH311 = cub_kemo
TARGET_CUBMESH322 = cub_kemo_quad
TARGET_INI_T_CUBE = gen_initial_4_plane
TARGET_INI_B_CUBE = add_magne_ini_4_plane

SOURCES = $(shell ls *.f90)

MOD_CUBMESH311 = \
cub_kemo.o \
\
m_ctl_data_4_cub_kemo.o \
t_control_param_plane_mesh.o

MOD_CUBMESH322 = \
cub_kemo_quad.o \
\
m_ctl_data_4_cub_kemo.o \
t_control_param_plane_mesh.o

MOD_INI_T_CUBE = \
generate_initial_4_plane.o \
\
m_ctl_data_4_cub_kemo.o \
t_control_param_plane_mesh.o

MOD_INI_B_CUBE = \
add_magne_ini_4_plane.o \
\
m_ctl_data_4_cub_kemo.o \
t_control_param_plane_mesh.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PLANE_MODEL_DIR =  $(PLANE_MODEL_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_CUBMESH311 = $$(BUILDDIR)/$(TARGET_CUBMESH311)'    \
	>> $(MAKENAME)
	@echo 'TARGET_CUBMESH322 = $$(BUILDDIR)/$(TARGET_CUBMESH322)'   \
	>> $(MAKENAME)
	@echo 'TARGET_INI_T_CUBE = $$(BUILDDIR)/$(TARGET_INI_T_CUBE)'   \
	>> $(MAKENAME)
	@echo 'TARGET_INI_B_CUBE = $$(BUILDDIR)/$(TARGET_INI_B_CUBE)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '' >> $(MAKENAME)
	@echo plane_mesh:    \
	'$$(TARGET_CUBMESH311)  $$(TARGET_CUBMESH322)'    \
	'$$(TARGET_INI_T_CUBE)  $$(TARGET_INI_B_CUBE)'    \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CUBMESH311)': '$$(MOD_CUBMESH311)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CUBMESH311)' \
	'$$(MOD_CUBMESH311)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_CUBMESH322)': '$$(MOD_CUBMESH322)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CUBMESH322)' \
	'$$(MOD_CUBMESH322)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_INI_T_CUBE)': '$$(MOD_INI_T_CUBE)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_INI_T_CUBE)' \
	'$$(MOD_INI_T_CUBE)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_INI_B_CUBE)': '$$(MOD_INI_B_CUBE)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_INI_B_CUBE)' \
	'$$(MOD_INI_B_CUBE)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_CUBMESH311=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBMESH311) >> $(MAKENAME)
	@echo  MOD_CUBMESH322=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBMESH322) >> $(MAKENAME)
	@echo  MOD_INI_T_CUBE=  \\ >> $(MAKENAME)
	@echo  $(MOD_INI_T_CUBE) >> $(MAKENAME)
	@echo  MOD_INI_B_CUBE=  \\ >> $(MAKENAME)
	@echo  $(MOD_INI_B_CUBE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PLANE_MODEL_DIR)' $(SOURCES)
