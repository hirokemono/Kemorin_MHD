#
#
#

GEN_SPH_GRIDDIR = $$(MESHER_DIR)/spherical_shell

TARGET_GEN_SPH_GRID =       gen_sph_grids_full
TARGET_GEN_SPH_NOSF =       gen_sph_grids_gaunt
TARGET_GEN_SPH_PARA =       parallel_gen_sph_grids
TARGET_GEN_EQ_GRID =        gen_eq_plane
TARGET_MAKE_SPH_RADIUS =    make_sph_mesh_radius
TARGET_COR_TRIINT =         gaunt_coriolis
TARGET_TEST_SPH_GRID =      check_sph_grids

SOURCES = $(shell ls *.f90)

MOD_GEN_SPH_GRID = \
generate_sph_grids.o

MOD_GEN_SPH_GRID_NOSF = \
generate_sph_grids_nosf.o

MOD_GEN_SPH_GRID_PARA = \
generate_sph_grids_para.o

MOD_GEN_EQ_GRID = \
generate_equator_grid.o

MOD_MAKE_SPH_RADIUS = \
const_radial_grid_sph.o

MOD_TEST_SPH_GRID = \
m_read_ctl_gen_sph_shell.o \
cmp_trans_sph_indices.o \
\
analyzer_test_sph.o \
main_test_sph.o

MOD_COR_TRIINT = \
gi_coriolis.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_SPH_GRIDDIR = $(GEN_SPH_GRIDDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_SPH_GRID =    $$(BUILDDIR)/$(TARGET_GEN_SPH_GRID)'    \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_SPH_NOSF =     $$(BUILDDIR)/$(TARGET_GEN_SPH_NOSF)'   \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_SPH_PARA =     $$(BUILDDIR)/$(TARGET_GEN_SPH_PARA)'   \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_EQ_GRID =     $$(BUILDDIR)/$(TARGET_GEN_EQ_GRID)'     \
	>> $(MAKENAME)
	@echo 'TARGET_MAKE_SPH_RADIUS = $$(BUILDDIR)/$(TARGET_MAKE_SPH_RADIUS)' \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_SPH_GRID = $$(BUILDDIR)/$(TARGET_TEST_SPH_GRID)'     \
	>> $(MAKENAME)
	@echo 'TARGET_COR_TRIINT = $$(BUILDDIR)/$(TARGET_COR_TRIINT)'     \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_utils:   \
	'$$(TARGET_GEN_SPH_GRID)    $$(TARGET_GEN_EQ_GRID)'   \
	'$$(TARGET_MAKE_SPH_RADIUS) $$(TARGET_TEST_SPH_GRID)' \
	'$$(TARGET_GEN_SPH_NOSF)    $$(TARGET_COR_TRIINT)'    \
	'$$(TARGET_GEN_SPH_PARA)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_GRID)': '$$(MOD_GEN_SPH_GRID)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_SPH_GRID)' \
	'$$(MOD_GEN_SPH_GRID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_NOSF)': '$$(MOD_GEN_SPH_GRID_NOSF)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_SPH_NOSF)' \
	'$$(MOD_GEN_SPH_GRID_NOSF)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_PARA)': '$$(MOD_GEN_SPH_GRID_PARA)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_SPH_PARA)' \
	'$$(MOD_GEN_SPH_GRID_PARA)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_EQ_GRID)': '$$(MOD_GEN_EQ_GRID)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_EQ_GRID)' \
	'$$(MOD_GEN_EQ_GRID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAKE_SPH_RADIUS): $$(MOD_MAKE_SPH_RADIUS)' \
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
	@echo '$$(TARGET_COR_TRIINT):' \
	'$$(MOD_COR_TRIINT) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_COR_TRIINT)' \
	'$$(MOD_COR_TRIINT)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_GEN_SPH_GRID_NOSF=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID_NOSF) >> $(MAKENAME)
	@echo  MOD_GEN_SPH_GRID_PARA=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID_PARA) >> $(MAKENAME)
	@echo  MOD_GEN_EQ_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_EQ_GRID) >> $(MAKENAME)
	@echo  MOD_MAKE_SPH_RADIUS=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAKE_SPH_RADIUS) >> $(MAKENAME)
	@echo  MOD_TEST_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_COR_TRIINT=  \\ >> $(MAKENAME)
	@echo  $(MOD_COR_TRIINT) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GEN_SPH_GRIDDIR)' $(SOURCES)
