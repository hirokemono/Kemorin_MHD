#
#
#

PSF_UTIL_MAINDIR = $$(DATA_UTILS_DIR)/PSF_utils

TARGET_PSF_AVES =  psf_average
TARGET_PSF_2_VTK = psf_2_vtk
TARGET_PSF_2_DX =  psf_2_dx

TARGET_DRAW_CZ =   drcz_fem
TARGET_DRAW_CX =   drcx_fem
TARGET_DRAW_CY =   drcy_fem
TARGET_DRAW_MAP =  drmap_fem


LIBS_PSF_UTIL = \
'$$(LIB_UTILS)' \
'$$(LIB_VIZ)'   \
'$$(LIB_MESH)'

LIB_FILES_PSF_UTIL = \
'$$(LIB_UTILS_FILE)' \
'$$(LIB_VIZ_FILE)'   \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_PSF_UTIL+=        '$$(LIB_KEMO_C)'
  LIB_FILES_PSF_UTIL+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_PSF_AVES = \
psf_rms_aves.o \
cal_psf_rms_aves.o \
\
m_norms_4_psf.o \
m_control_data_psf_utils.o \
take_avarages_4_psf.o \
take_normals_4_psf.o

MOD_PSF_2_VTK = \
psf_2_vtk.o \
m_norms_4_psf.o \
m_control_data_psf_utils.o \
take_avarages_4_psf.o \
convert_psf_file.o

MOD_PSF_2_DX = \
psf_2_dx.o \
m_norms_4_psf.o \
m_control_data_psf_utils.o \
take_avarages_4_psf.o \
convert_psf_file.o


MOD_ZCPLANE_PG = \
drcz_fem.o \
m_norms_4_psf.o \
take_avarages_4_psf.o


MOD_XCPLANE_PG = \
drcx_fem.o \
m_norms_4_psf.o \
take_avarages_4_psf.o

MOD_YCPLANE_PG = \
drcy_fem.o \
m_norms_4_psf.o \
take_avarages_4_psf.o

MOD_MAP_PG = \
drmap_fem.o \
m_norms_4_psf.o \
take_avarages_4_psf.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_UTIL_MAINDIR = $(PSF_UTIL_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_PSF_AVES =     $$(BUILDDIR)/$(TARGET_PSF_AVES)'     >> $(MAKENAME)
	@echo 'TARGET_PSF_2_VTK =    $$(BUILDDIR)/$(TARGET_PSF_2_VTK)'    >> $(MAKENAME)
	@echo 'TARGET_PSF_2_DX =     $$(BUILDDIR)/$(TARGET_PSF_2_DX)'     >> $(MAKENAME)
	@echo 'TARGET_DRAW_CZ =      $$(BUILDDIR)/$(TARGET_DRAW_CZ)'      >> $(MAKENAME)
	@echo 'TARGET_DRAW_CX =      $$(BUILDDIR)/$(TARGET_DRAW_CX)'      >> $(MAKENAME)
	@echo 'TARGET_DRAW_CY =      $$(BUILDDIR)/$(TARGET_DRAW_CY)'      >> $(MAKENAME)
	@echo 'TARGET_DRAW_MAP =     $$(BUILDDIR)/$(TARGET_DRAW_MAP)'     >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo psf_utils: \
	'$$(TARGET_PSF_2_VTK)    $$(TARGET_PSF_2_DX) '         \
	'$$(TARGET_PSF_AVES) ' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo utils_pg: \
	'$$(TARGET_DRAW_CZ) $$(TARGET_DRAW_MAP) ' \
	'$$(TARGET_DRAW_CX) $$(TARGET_DRAW_CY) '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_AVES)': '$$(MOD_PSF_AVES)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PSF_AVES)' \
	'$$(MOD_PSF_AVES)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_2_VTK)': '$$(MOD_PSF_2_VTK)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PSF_2_VTK)' \
	'$$(MOD_PSF_2_VTK)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_2_DX)': '$$(MOD_PSF_2_DX)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PSF_2_DX)' \
	'$$(MOD_PSF_2_DX)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CZ)': '$$(MOD_ZCPLANE_PG)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DRAW_CZ)' \
	'$$(MOD_ZCPLANE_PG)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CX)': '$$(MOD_XCPLANE_PG)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DRAW_CX)' \
	'$$(MOD_XCPLANE_PG)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_CY)': '$$(MOD_YCPLANE_PG)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DRAW_CY)' \
	'$$(MOD_YCPLANE_PG)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DRAW_MAP)': '$$(MOD_MAP_PG)' \
	'$$(LIB_FILES_PSF_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DRAW_MAP)' \
	'$$(MOD_MAP_PG)' '-L. $$(LIBS_PSF_UTIL)' \
	'$$(PGPLOT_LIBS) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_PSF_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_PSF_UTIL)            >> $(MAKENAME)
	@echo  LIB_FILES_PSF_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PSF_UTIL)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_PSF_AVES =  \\   >> $(MAKENAME)
	@echo  $(MOD_PSF_AVES)      >> $(MAKENAME)
	@echo  MOD_PSF_2_VTK =  \\  >> $(MAKENAME)
	@echo  $(MOD_PSF_2_VTK)     >> $(MAKENAME)
	@echo  MOD_PSF_2_DX =  \\   >> $(MAKENAME)
	@echo  $(MOD_PSF_2_DX)      >> $(MAKENAME)
	@echo  MOD_ZCPLANE_PG =  \\ >> $(MAKENAME)
	@echo  $(MOD_ZCPLANE_PG)    >> $(MAKENAME)
	@echo  MOD_XCPLANE_PG =  \\ >> $(MAKENAME)
	@echo  $(MOD_XCPLANE_PG)    >> $(MAKENAME)
	@echo  MOD_YCPLANE_PG =  \\ >> $(MAKENAME)
	@echo  $(MOD_YCPLANE_PG)    >> $(MAKENAME)
	@echo  MOD_MAP_PG =  \\     >> $(MAKENAME)
	@echo  $(MOD_MAP_PG)        >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PSF_UTIL_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
