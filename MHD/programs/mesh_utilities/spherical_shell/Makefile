#
#
#

GEN_SPH_GRIDDIR = $$(MESHER_DIR)/spherical_shell

TARGET_GEN_SPH_GRID =       gen_sph_grids
TARGET_MAKE_SPH_RADIUS =    make_sph_mesh_radius
TARGET_TEST_SPH_GRID =      check_sph_grids
TARGET_FEM_MESH_RAYLEIGH =  make_rayleigh_mesh

SOURCES = $(shell ls *.f90)

MOD_GEN_SPH_GRID = \
main_generate_sph_grids.o \
analyzer_gen_sph_grids.o

MOD_MAKE_SPH_RADIUS = \
const_radial_grid_sph.o

MOD_TEST_SPH_GRID = \
cmp_trans_sph_indices.o \
cmp_trans_sph_tests.o \
\
analyzer_test_sph.o \
main_test_sph.o

MOD_FEM_MESH_RAYLEIGH = \
main_gen_FEM_4_rayleigh.o \
analyzer_gen_FEM_4_rayleigh.o \
\
const_fem_nodes_4_rayleigh.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_SPH_GRIDDIR = $(GEN_SPH_GRIDDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_SPH_GRID =     $$(BUILDDIR)/$(TARGET_GEN_SPH_GRID)'   \
	>> $(MAKENAME)
	@echo 'TARGET_MAKE_SPH_RADIUS = $$(BUILDDIR)/$(TARGET_MAKE_SPH_RADIUS)' \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_SPH_GRID = $$(BUILDDIR)/$(TARGET_TEST_SPH_GRID)'     \
	>> $(MAKENAME)
	@echo 'TARGET_FEM_MESH_RAYLEIGH = $$(BUILDDIR)/$(TARGET_FEM_MESH_RAYLEIGH)'     \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_utils:   \
	'$$(TARGET_GEN_SPH_GRID)   $$(TARGET_MAKE_SPH_RADIUS)'   \
	'$$(TARGET_TEST_SPH_GRID)  $$(TARGET_FEM_MESH_RAYLEIGH)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_GRID)': '$$(MOD_GEN_SPH_GRID)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_SPH_GRID)' \
	'$$(MOD_GEN_SPH_GRID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAKE_SPH_RADIUS): $$(MOD_MAKE_SPH_RADIUS)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MAKE_SPH_RADIUS)' \
	'$$(MOD_MAKE_SPH_RADIUS)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_SPH_GRID):' \
	'$$(MOD_TEST_SPH_GRID) $$(LIB_FILES_MESH_CHECK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TEST_SPH_GRID)' \
	'$$(MOD_TEST_SPH_GRID)' \
	'-L. $$(LIBS_MESH_CHECK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FEM_MESH_RAYLEIGH)': '$$(MOD_FEM_MESH_RAYLEIGH)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_FEM_MESH_RAYLEIGH)' \
	'$$(MOD_FEM_MESH_RAYLEIGH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_MAKE_SPH_RADIUS=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAKE_SPH_RADIUS) >> $(MAKENAME)
	@echo  MOD_TEST_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_FEM_MESH_RAYLEIGH=  \\ >> $(MAKENAME)
	@echo  $(MOD_FEM_MESH_RAYLEIGH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GEN_SPH_GRIDDIR)' $(SOURCES)
