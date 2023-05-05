#
#
#

GEN_SPH_GRIDDIR = $$(MESHER_DIR)/spherical_shell

TARGET_GEN_SPH_GRID =       gen_sph_grids
TARGET_GEN_SPH_W_LIC =      gen_sph_w_repart

TARGET_CHECK_SPH_GRID =     tests/check_sph_grids
TARGET_MAKE_SPH_RADIUS =    tests/make_sph_mesh_radius
TARGET_TEST_SPH_GRID =      tests/check_sph_comms
TARGET_GEN_SPH_GRID_TEST =  tests/gen_sph_grid_test

SOURCES = $(shell ls *.f90)

MOD_GEN_SPH_GRID = \
main_generate_sph_grids.o \
analyzer_gen_sph_grids.o \
input_control_const_shell.o

MOD_GEN_SPH_W_LIC = \
t_ctl_data_gen_sph_w_repart.o \
input_ctl_gen_sph_w_repart.o \
main_gen_sph_w_repart.o \
analyzer_gen_sph_w_repart.o

MOD_CHECK_SPH_GRID = \
input_control_const_shell.o \
main_check_sph_grids.o \
analyzer_check_sph_grids.o

MOD_MAKE_SPH_RADIUS = \
const_radial_grid_sph.o

MOD_TEST_SPH_GRID = \
cmp_trans_sph_indices.o \
cmp_trans_sph_tests.o \
\
input_control_const_shell.o \
analyzer_test_sph.o \
main_test_sph.o

MOD_GEN_SPH_GRID_TEST = \
input_control_const_shell.o \
main_generate_sph_grid_t.o \
analyzer_gen_sph_grid_t.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_SPH_GRIDDIR = $(GEN_SPH_GRIDDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_SPH_GRID =     $$(BUILDDIR)/$(TARGET_GEN_SPH_GRID)'   \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_SPH_W_LIC =    $$(BUILDDIR)/$(TARGET_GEN_SPH_W_LIC)'   \
	>> $(MAKENAME)
	@echo 'TARGET_CHECK_SPH_GRID =   $$(BUILDDIR)/$(TARGET_CHECK_SPH_GRID)'   \
	>> $(MAKENAME)
	@echo 'TARGET_MAKE_SPH_RADIUS = $$(BUILDDIR)/$(TARGET_MAKE_SPH_RADIUS)' \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_SPH_GRID = $$(BUILDDIR)/$(TARGET_TEST_SPH_GRID)'     \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_SPH_GRID_TEST =     $$(BUILDDIR)/$(TARGET_GEN_SPH_GRID_TEST)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_utils:   \
	'$$(TARGET_GEN_SPH_GRID)     $$(TARGET_GEN_SPH_W_LIC)' \
	'$$(TARGET_MAKE_SPH_RADIUS)  $$(TARGET_TEST_SPH_GRID)'  \
	'$$(TARGET_GEN_SPH_GRID_TEST) $$(TARGET_CHECK_SPH_GRID)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_GRID)': '$$(MOD_GEN_SPH_GRID)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GEN_SPH_GRID)' \
	'$$(MOD_GEN_SPH_GRID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_W_LIC)': '$$(MOD_GEN_SPH_W_LIC)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GEN_SPH_W_LIC)' \
	'$$(MOD_GEN_SPH_W_LIC)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CHECK_SPH_GRID)': '$$(MOD_CHECK_SPH_GRID)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CHECK_SPH_GRID)' \
	'$$(MOD_CHECK_SPH_GRID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAKE_SPH_RADIUS): $$(MOD_MAKE_SPH_RADIUS)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_MAKE_SPH_RADIUS)' \
	'$$(MOD_MAKE_SPH_RADIUS)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_SPH_GRID):' \
	'$$(MOD_TEST_SPH_GRID) $$(LIB_FILES_MESH_CHECK)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_TEST_SPH_GRID)' \
	'$$(MOD_TEST_SPH_GRID)' \
	'-L. $$(LIBS_MESH_CHECK)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_GRID_TEST)': '$$(MOD_GEN_SPH_GRID_TEST)' \
	'$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GEN_SPH_GRID_TEST)' \
	'$$(MOD_GEN_SPH_GRID_TEST)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_GEN_SPH_W_LIC=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_W_LIC) >> $(MAKENAME)
	@echo  MOD_CHECK_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_CHECK_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_MAKE_SPH_RADIUS=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAKE_SPH_RADIUS) >> $(MAKENAME)
	@echo  MOD_TEST_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_GEN_SPH_GRID_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID_TEST) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(GEN_SPH_GRIDDIR)' $(SOURCES)
