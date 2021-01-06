#
#
#

VIZ_PLUG_DIR = $$(PROG_DIR)/VIZ_only

TARGET_VIZ =         viz_kemo
TARGET_PSF =         sectioning
TARGET_PVR =         pvr_kemo
TARGET_FLINE =       fieldline_kemo
TARGET_VTK_CONVERT = field_to_VTK

LIBS_VIZ = \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'   \
'$$(LIB_MT_STREAM)'

LIB_FILES_VIZ = \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_MT_STREAM_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_VIZ+=        '$$(LIB_KEMO_C)'
  LIB_FILES_VIZ+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90)

MOD_VIZ_PLUG =  \
main_visualization.o  \
analyzer_viz.o \
FEM_analyzer_viz.o \
t_visualization.o \
t_control_data_all_vizs.o \
t_field_list_for_vizs.o \
load_mesh_and_field_4_viz.o

MOD_PSF_PLUG = \
main_surface_rendering.o \
analyzer_psf.o \
FEM_analyzer_viz_surf.o \
t_surfacing.o \
t_control_data_section_only.o \
t_field_list_for_vizs.o \
load_mesh_and_field_4_viz.o

MOD_PVR_PLUG =  \
main_pvr.o  \
analyzer_pvr.o \
FEM_analyzer_viz_pvr.o \
t_visualization.o \
t_control_data_all_vizs.o \
t_field_list_for_vizs.o \
load_mesh_and_field_4_viz.o

MOD_FLINE_PLUG = \
main_fieldline.o \
analyzer_fline.o \
FEM_analyzer_viz_fline.o \
t_visualization.o \
t_control_data_all_vizs.o \
t_field_list_for_vizs.o \
load_mesh_and_field_4_viz.o

MOD_VTK_CONVERT_PLUG = \
main_VTK_convert.o \
analyzer_VTK_convert.o \
FEM_analyzer_VTK_convert.o \
t_viz_VTK_convert.o \
t_control_data_section_only.o \
t_field_list_for_vizs.o \
load_mesh_and_field_4_viz.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_PLUG_DIR =  $(VIZ_PLUG_DIR)'         >> $(MAKENAME)

target_list:
	@echo 'TARGET_PSF =         $$(BUILDDIR)/$(TARGET_PSF)' >> $(MAKENAME)
	@echo 'TARGET_VIZ =         $$(BUILDDIR)/$(TARGET_VIZ)' >> $(MAKENAME)
	@echo 'TARGET_PVR =         $$(BUILDDIR)/$(TARGET_PVR)' >> $(MAKENAME)
	@echo 'TARGET_FLINE =       $$(BUILDDIR)/$(TARGET_FLINE)' >> $(MAKENAME)
	@echo 'TARGET_VTK_CONVERT = $$(BUILDDIR)/$(TARGET_VTK_CONVERT)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo visualizer: \
	'$$(TARGET_VIZ)' \
	'$$(TARGET_PSF)' \
	'$$(TARGET_PVR)' \
	'$$(TARGET_FLINE)' \
	'$$(TARGET_VTK_CONVERT)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VIZ):  $$(MOD_VIZ_PLUG)' \
	'$$(LIB_FILES_VIZ)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VIZ)' '$$(MOD_VIZ_PLUG)' \
	'-L. $$(LIBS_VIZ) $$(F90LIBS) $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF): $$(MOD_PSF_PLUG)' \
	'$$(LIB_FILES_VIZ)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF)' '$$(MOD_PSF_PLUG)' \
	'-L. $$(LIBS_VIZ) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FLINE): $$(MOD_FLINE_PLUG)' \
	'$$(LIB_FILES_VIZ)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FLINE)' '$$(MOD_FLINE_PLUG)' \
	'-L. $$(LIBS_VIZ) $$(F90LIBS) $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PVR):  $$(MOD_PVR_PLUG)' \
	'$$(LIB_FILES_VIZ)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PVR)' '$$(MOD_PVR_PLUG)' \
	'-L. $$(LIBS_VIZ) $$(F90LIBS) $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VTK_CONVERT):  $$(MOD_VTK_CONVERT_PLUG)' \
	'$$(LIB_FILES_VIZ)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VTK_CONVERT)' '$$(MOD_VTK_CONVERT_PLUG)' \
	'-L. $$(LIBS_VIZ) $$(F90LIBS) $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:
	@echo  LIBS_VIZ=  \\          >> $(MAKENAME)
	@echo  $(LIBS_VIZ)            >> $(MAKENAME)
	@echo  LIB_FILES_VIZ=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_VIZ)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


mod_list:
	@echo  MOD_PSF_PLUG=  \\         >> $(MAKENAME)
	@echo  $(MOD_PSF_PLUG)           >> $(MAKENAME)
	@echo  MOD_VIZ_PLUG=  \\         >> $(MAKENAME)
	@echo  $(MOD_VIZ_PLUG)           >> $(MAKENAME)
	@echo  MOD_PVR_PLUG=  \\         >> $(MAKENAME)
	@echo  $(MOD_PVR_PLUG)           >> $(MAKENAME)
	@echo  MOD_FLINE_PLUG=  \\       >> $(MAKENAME)
	@echo  $(MOD_FLINE_PLUG)         >> $(MAKENAME)
	@echo  MOD_VTK_CONVERT_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_VTK_CONVERT_PLUG)   >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(VIZ_PLUG_DIR)' $(SOURCES)
