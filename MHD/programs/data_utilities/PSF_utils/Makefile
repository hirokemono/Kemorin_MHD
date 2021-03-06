#
#
#

PSF_UTIL_MAINDIR = $$(DATA_UTILS_DIR)/PSF_utils

TARGET_PSF_2_VTK = psf_to_vtk
TARGET_PSF_AVES =  psf_average
TARGET_PSF_RANGE_AVES =  psf_range_ave

TARGET_DRAW_CZ =   drcz_fem
TARGET_DRAW_CX =   drcx_fem
TARGET_DRAW_CY =   drcy_fem
TARGET_DRAW_MAP =  drmap_fem

TARGET_LINE_ON_PSF =   section_from_psf
TARGET_DBENCH_ON_PSF = psf_dynamobench

TARGET_FIX_TEMP_PSF =      psf_per_temp_2_temp
TARGET_FIX_SGS_MF_WK_PSF = psf_fix_reynolds_wk

TARGET_UPDATE_PSF_BIN = update_psf_bin

TARGET_COMPARE_PSF = compare_psf


LIBS_PSF_UTIL = \
'$$(LIB_UTILS)'   \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_PSF_UTIL = \
'$$(LIB_UTILS_FILE)'  \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

LIBS_PG_UTIL = \
'$$(LIB_DRAW_PG)' \
'$$(LIB_MESH)'

LIB_FILES_PG_UTIL = \
'$$(LIB_DRAW_PG_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_PSF_UTIL+=        '$$(LIB_KEMO_C)'
  LIB_FILES_PSF_UTIL+=   '$$(LIB_KEMO_C_FILE)'
  LIBS_PG_UTIL+=         '$$(LIB_KEMO_C)'
  LIB_FILES_PG_UTIL+=    '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90 *.F90)

MOD_PSF_2_VTK = \
psf_file_to_VTK.o

MOD_UPDATE_PSF_BIN = \
update_old_psf_binary.o \
sel_read_old_binary_ucd.o

MOD_PSF_AVES = \
psf_rms_aves.o \
cal_psf_rms_aves.o \
\
m_psf_results.o \
t_norms_4_psf.o \
take_avarages_4_psf.o \
take_normals_4_psf.o

MOD_PSF_RANGE_AVES = \
psf_range_rms_aves.o \
cal_psf_rms_aves.o \
\
m_psf_results.o \
t_norms_4_psf.o \
take_avarages_4_psf.o \
take_normals_4_psf.o


MOD_ZCPLANE_PG = \
drcz_fem.o \
set_rgb_colors.o \
m_psf_results.o \
t_norms_4_psf.o


MOD_XCPLANE_PG = \
drcx_fem.o \
set_rgb_colors.o \
m_psf_results.o \
t_norms_4_psf.o

MOD_YCPLANE_PG = \
drcy_fem.o \
set_rgb_colors.o \
m_psf_results.o \
t_norms_4_psf.o

MOD_MAP_PG = \
drmap_fem.o \
set_rgb_colors.o \
m_psf_results.o \
t_norms_4_psf.o

MOD_LINE_ON_PSF = \
section_from_psf.o \
m_psf_results.o \
t_norms_4_psf.o \
m_psf_edge_connect.o \
m_line_from_psf.o \
set_node_on_edge_quad_psf.o \
const_section_from_triangle.o

MOD_DYNAMOBENCH_PSF = \
psf_dynamobench.o \
m_psf_results.o \
t_norms_4_psf.o \
m_psf_edge_connect.o \
m_line_from_psf.o \
set_node_on_edge_quad_psf.o \
const_section_from_triangle.o \
m_dynamobench_data_by_psf.o

MOD_FIX_TEMP_PSF = \
t_norms_4_psf.o \
m_psf_results.o \
fix_temperature_4_psf.o

MOD_FIX_SGS_MF_WK_PSF = \
psf_fix_reynods_work.o \
m_psf_results.o


MOD_COMPARE_PSF = \
compare_psf_files.o \
t_ctl_param_psf_compares.o \
t_ctl_data_psf_compares.o \
t_ctl_data_psf_compare.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_UTIL_MAINDIR = $(PSF_UTIL_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_PSF_2_VTK =    $$(BUILDDIR)/$(TARGET_PSF_2_VTK)'        >> $(MAKENAME)
	@echo 'TARGET_PSF_AVES =     $$(BUILDDIR)/$(TARGET_PSF_AVES)'         >> $(MAKENAME)
	@echo 'TARGET_PSF_RANGE_AVES = $$(BUILDDIR)/$(TARGET_PSF_RANGE_AVES)' >> $(MAKENAME)
	@echo 'TARGET_DRAW_CZ =      $$(BUILDDIR)/$(TARGET_DRAW_CZ)'          >> $(MAKENAME)
	@echo 'TARGET_DRAW_CX =      $$(BUILDDIR)/$(TARGET_DRAW_CX)'          >> $(MAKENAME)
	@echo 'TARGET_DRAW_CY =      $$(BUILDDIR)/$(TARGET_DRAW_CY)'          >> $(MAKENAME)
	@echo 'TARGET_DRAW_MAP =     $$(BUILDDIR)/$(TARGET_DRAW_MAP)'         >> $(MAKENAME)
	@echo 'TARGET_LINE_ON_PSF =  $$(BUILDDIR)/$(TARGET_LINE_ON_PSF)'      >> $(MAKENAME)
	@echo 'TARGET_DBENCH_ON_PSF = $$(BUILDDIR)/$(TARGET_DBENCH_ON_PSF)'   >> $(MAKENAME)
	@echo 'TARGET_COMPARE_PSF =   $$(BUILDDIR)/$(TARGET_COMPARE_PSF)'     >> $(MAKENAME)
	@echo 'TARGET_FIX_TEMP_PSF =  $$(BUILDDIR)/$(TARGET_FIX_TEMP_PSF)'    >> $(MAKENAME)
	@echo 'TARGET_FIX_SGS_MF_WK_PSF =  $$(BUILDDIR)/$(TARGET_FIX_SGS_MF_WK_PSF)'    >> $(MAKENAME)
	@echo 'TARGET_UPDATE_PSF_BIN = $$(BUILDDIR)/$(TARGET_UPDATE_PSF_BIN)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo psf_utils: \
	'$$(TARGET_PSF_2_VTK)      $$(TARGET_PSF_AVES)'     \
	'$$(TARGET_PSF_RANGE_AVES) $$(TARGET_LINE_ON_PSF)'  \
	'$$(TARGET_DBENCH_ON_PSF)  $$(TARGET_UPDATE_PSF_BIN)' \
	'$$(TARGET_COMPARE_PSF)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo utils_pg: \
	'$$(TARGET_DRAW_CZ) $$(TARGET_DRAW_MAP) ' \
	'$$(TARGET_DRAW_CX) $$(TARGET_DRAW_CY)  ' \
	'$$(TARGET_FIX_TEMP_PSF) $$(TARGET_FIX_SGS_MF_WK_PSF)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_2_VTK)': '$$(MOD_PSF_2_VTK)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF_2_VTK)' \
	'$$(MOD_PSF_2_VTK)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_UPDATE_PSF_BIN)': '$$(MOD_UPDATE_PSF_BIN)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_UPDATE_PSF_BIN)' \
	'$$(MOD_UPDATE_PSF_BIN)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_AVES)': '$$(MOD_PSF_AVES)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF_AVES)' \
	'$$(MOD_PSF_AVES)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_RANGE_AVES)': '$$(MOD_PSF_RANGE_AVES)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF_RANGE_AVES)' \
	'$$(MOD_PSF_RANGE_AVES)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CZ)': '$$(MOD_ZCPLANE_PG)' \
	'$$(LIB_FILES_PG_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90_LOCAL)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DRAW_CZ)' \
	'$$(MOD_ZCPLANE_PG)' '-L. $$(LIBS_PG_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CX)': '$$(MOD_XCPLANE_PG)' \
	'$$(LIB_FILES_PG_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90_LOCAL)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DRAW_CX)' \
	'$$(MOD_XCPLANE_PG)' '-L. $$(LIBS_PG_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CY)': '$$(MOD_YCPLANE_PG)' \
	'$$(LIB_FILES_PG_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90_LOCAL)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DRAW_CY)' \
	'$$(MOD_YCPLANE_PG)' '-L. $$(LIBS_PG_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_MAP)': '$$(MOD_MAP_PG)' \
	'$$(LIB_FILES_PG_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90_LOCAL)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DRAW_MAP)' \
	'$$(MOD_MAP_PG)' '-L. $$(LIBS_PG_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_LINE_ON_PSF)': '$$(MOD_LINE_ON_PSF)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_LINE_ON_PSF)' \
	'$$(MOD_LINE_ON_PSF)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DBENCH_ON_PSF)': '$$(MOD_DYNAMOBENCH_PSF)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DBENCH_ON_PSF)' \
	'$$(MOD_DYNAMOBENCH_PSF)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FIX_TEMP_PSF)': '$$(MOD_FIX_TEMP_PSF)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FIX_TEMP_PSF)' \
	'$$(MOD_FIX_TEMP_PSF)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FIX_SGS_MF_WK_PSF)': '$$(MOD_FIX_SGS_MF_WK_PSF)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FIX_SGS_MF_WK_PSF)' \
	'$$(MOD_FIX_SGS_MF_WK_PSF)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_COMPARE_PSF)': '$$(MOD_COMPARE_PSF)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_COMPARE_PSF)' \
	'$$(MOD_COMPARE_PSF)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_PSF_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_PSF_UTIL)            >> $(MAKENAME)
	@echo  LIB_FILES_PSF_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PSF_UTIL)       >> $(MAKENAME)
	@echo  LIBS_PG_UTIL=  \\           >> $(MAKENAME)
	@echo  $(LIBS_PG_UTIL)             >> $(MAKENAME)
	@echo  LIB_FILES_PG_UTIL=  \\      >> $(MAKENAME)
	@echo  $(LIB_FILES_PG_UTIL)        >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_PSF_2_VTK =  \\       >> $(MAKENAME)
	@echo  $(MOD_PSF_2_VTK)          >> $(MAKENAME)
	@echo  MOD_UPDATE_PSF_BIN =  \\  >> $(MAKENAME)
	@echo  $(MOD_UPDATE_PSF_BIN)     >> $(MAKENAME)
	@echo  MOD_PSF_AVES =  \\        >> $(MAKENAME)
	@echo  $(MOD_PSF_AVES)           >> $(MAKENAME)
	@echo  MOD_PSF_RANGE_AVES =  \\  >> $(MAKENAME)
	@echo  $(MOD_PSF_RANGE_AVES)     >> $(MAKENAME)
	@echo  MOD_ZCPLANE_PG =  \\      >> $(MAKENAME)
	@echo  $(MOD_ZCPLANE_PG)         >> $(MAKENAME)
	@echo  MOD_XCPLANE_PG =  \\      >> $(MAKENAME)
	@echo  $(MOD_XCPLANE_PG)         >> $(MAKENAME)
	@echo  MOD_YCPLANE_PG =  \\      >> $(MAKENAME)
	@echo  $(MOD_YCPLANE_PG)         >> $(MAKENAME)
	@echo  MOD_MAP_PG =  \\          >> $(MAKENAME)
	@echo  $(MOD_MAP_PG)             >> $(MAKENAME)
	@echo  MOD_LINE_ON_PSF =  \\     >> $(MAKENAME)
	@echo  $(MOD_LINE_ON_PSF)        >> $(MAKENAME)
	@echo  MOD_DYNAMOBENCH_PSF =  \\ >> $(MAKENAME)
	@echo  $(MOD_DYNAMOBENCH_PSF)    >> $(MAKENAME)
	@echo  MOD_COMPARE_PSF =  \\     >> $(MAKENAME)
	@echo  $(MOD_COMPARE_PSF)        >> $(MAKENAME)
	@echo  MOD_FIX_TEMP_PSF =  \\    >> $(MAKENAME)
	@echo  $(MOD_FIX_TEMP_PSF)       >> $(MAKENAME)
	@echo  MOD_FIX_SGS_MF_WK_PSF =  \\    >> $(MAKENAME)
	@echo  $(MOD_FIX_SGS_MF_WK_PSF)       >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PSF_UTIL_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
