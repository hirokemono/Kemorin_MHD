#
#
#

CUBED_SPH_MESHDIR =  $$(CUBE_SPH_DIR)/cubed_sph_mesh

TARGET_CUBED_SPH =  cubed_sphere_kemo
TARGET_RECT_SPH =   rect_sphere_kemo

TARGET_SQURE_CIRC = circle_kemo

SOURCES = $(shell ls *.f90)

MOD_CUBED_SPH_MESH = \
mesh_core_kemo.o \
\
const_cube_sphere_surface.o \
const_cube_sphere_data.o

MOD_CUBED_SPH2_MESH = \
mesh_core_kemo2.o \
\
const_rect_sphere_surface.o \
const_rect_sphere_data.o


MOD_SQURE_CIRC_MESH = \
mesh_circle_kemo.o \
\
const_squre_circle_edge.o \
construct_circle_data.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBED_SPH_MESHDIR =  $(CUBED_SPH_MESHDIR)'  >> $(MAKENAME)

target_list:
	@echo 'TARGET_CUBED_SPH =  $$(BUILDDIR)/$(TARGET_CUBED_SPH)'   >> $(MAKENAME)
	@echo 'TARGET_RECT_SPH =   $$(BUILDDIR)/$(TARGET_RECT_SPH)'    >> $(MAKENAME)
	@echo 'TARGET_SQURE_CIRC = $$(BUILDDIR)/$(TARGET_SQURE_CIRC)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_CUBED_SPH)': '$$(MOD_CUBED_SPH_MESH)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CUBED_SPH)' \
	'$$(MOD_CUBED_SPH_MESH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_RECT_SPH)': '$$(MOD_CUBED_SPH2_MESH)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_RECT_SPH)' \
	'$$(MOD_CUBED_SPH2_MESH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SQURE_CIRC)': '$$(MOD_SQURE_CIRC_MESH)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SQURE_CIRC)' \
	'$$(MOD_SQURE_CIRC_MESH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_CUBED_SPH_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBED_SPH_MESH)   >> $(MAKENAME)
	@echo  MOD_CUBED_SPH2_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUBED_SPH2_MESH)   >> $(MAKENAME)
	@echo  MOD_SQURE_CIRC_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_SQURE_CIRC_MESH)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CUBED_SPH_MESHDIR)' $(SOURCES)
