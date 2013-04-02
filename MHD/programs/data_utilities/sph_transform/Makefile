#
#
#

SPH_TRANSFORM_DIR = $$(DATA_UTILS_DIR)/sph_transform

TARGET_SPH_TRANS =         sph_transform
TARGET_SPH_B_TRANS =       sph_back_transform
TARGET_GAUSS_B_TRANS =     sph_gauss_back_trans
TARGET_ZMEAN_TRANS =       zonal_mean_transform
TARGET_PICK_ZM_FLD_SPH =   pick_zonal_mean_sph_fld
TARGET_ZRMS_SPH_DATA =     zonal_rms_sph_grid

SOURCES = $(shell ls *.f90)

MOD_SPH_TRANS = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
analyzer_sph_transform.o \
FEM_analyzer_sph_trans.o \
SPH_analyzer_sph_trans.o \
main_sph_transform.o

MOD_SPH_B_TRANS = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
analyzer_sph_back_trans.o \
SPH_analyzer_back_trans.o \
FEM_analyzer_back_trans.o \
visualizer_all.o \
m_control_data_vizs.o  \
main_sph_back_trans.o

MOD_GAUSS_B_TRANS = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
analyzer_gauss_back_trans.o \
FEM_analyzer_back_trans.o \
SPH_analyzer_gauss_b_trans.o \
visualizer_all.o \
m_control_data_vizs.o  \
main_gauss_back_trans.o

MOD_ZM_F_TRANS = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
analyzer_zm_transform.o \
SPH_analyzer_sph_trans.o \
FEM_analyzer_sph_trans.o \
main_zm_transform.o


MOD_PICK_ZM_FLD_SPH = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
\
FEM_analyzer_sph_trans.o \
FEM_analyzer_back_trans.o \
SPH_analyzer_sph_trans.o \
SPH_analyzer_back_trans.o \
\
analyzer_zm_sph_field.o \
visualizer_all.o \
m_control_data_vizs.o  \
main_zm_sph_field.o


MOD_ZRMS_SPH_SPEC = \
m_ctl_data_4_sph_trans.o \
m_ctl_params_sph_trans.o \
\
copy_all_field_4_sph_trans.o \
sph_transfer_all_field.o \
\
FEM_analyzer_sph_trans.o \
FEM_analyzer_back_trans.o \
SPH_analyzer_sph_trans.o \
SPH_analyzer_back_trans.o \
\
visualizer_all.o \
m_control_data_vizs.o  \
analyzer_zonal_rms_sph.o \
main_zonal_rms_sph.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_TRANSFORM_DIR = $(SPH_TRANSFORM_DIR)'     >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_TRANS =       $$(BUILDDIR)/$(TARGET_SPH_TRANS)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_B_TRANS =     $$(BUILDDIR)/$(TARGET_SPH_B_TRANS)' \
	>> $(MAKENAME)
	@echo 'TARGET_GAUSS_B_TRANS =   $$(BUILDDIR)/$(TARGET_GAUSS_B_TRANS)' \
	>> $(MAKENAME)
	@echo 'TARGET_ZMEAN_TRANS =     $$(BUILDDIR)/$(TARGET_ZMEAN_TRANS)' \
	>> $(MAKENAME)
	@echo 'TARGET_PICK_ZM_FLD_SPH = $$(BUILDDIR)/$(TARGET_PICK_ZM_FLD_SPH)' \
	>> $(MAKENAME)
	@echo 'TARGET_ZRMS_SPH_DATA =   $$(BUILDDIR)/$(TARGET_ZRMS_SPH_DATA)' \
	>> $(MAKENAME)

target_task:
	@echo sph_trans: \
	'$$(TARGET_SPH_TRANS)          $$(TARGET_SPH_B_TRANS)'   \
	'$$(TARGET_GAUSS_B_TRANS)      $$(TARGET_ZMEAN_TRANS)'   \
	'$$(TARGET_PICK_ZM_FLD_SPH)    $$(TARGET_ZRMS_SPH_DATA)' >> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_TRANS)': '$$(MOD_SPH_TRANS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_TRANS)' \
	'$$(MOD_SPH_TRANS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_B_TRANS):  $$(MOD_SPH_B_TRANS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_B_TRANS)' \
	'$$(MOD_SPH_B_TRANS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GAUSS_B_TRANS):  $$(MOD_GAUSS_B_TRANS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GAUSS_B_TRANS)' \
	'$$(MOD_GAUSS_B_TRANS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ZMEAN_TRANS)': '$$(MOD_ZM_F_TRANS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ZMEAN_TRANS)' \
	'$$(MOD_ZM_F_TRANS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_ZM_FLD_SPH)': '$$(MOD_PICK_ZM_FLD_SPH)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_PICK_ZM_FLD_SPH)' \
	'$$(MOD_PICK_ZM_FLD_SPH)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ZRMS_SPH_DATA)': '$$(MOD_ZRMS_SPH_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ZRMS_SPH_DATA)' \
	'$$(MOD_ZRMS_SPH_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SPH_TRANS=  \\       >> $(MAKENAME)
	@echo  $(MOD_SPH_TRANS)         >> $(MAKENAME)
	@echo  MOD_SPH_B_TRANS=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_B_TRANS)       >> $(MAKENAME)
	@echo  MOD_GAUSS_B_TRANS=  \\   >> $(MAKENAME)
	@echo  $(MOD_GAUSS_B_TRANS)     >> $(MAKENAME)
	@echo  MOD_ZM_F_TRANS=  \\      >> $(MAKENAME)
	@echo  $(MOD_ZM_F_TRANS)        >> $(MAKENAME)
	@echo  MOD_PICK_ZM_FLD_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_PICK_ZM_FLD_SPH)   >> $(MAKENAME)
	@echo  MOD_ZRMS_SPH_SPEC=  \\   >> $(MAKENAME)
	@echo  $(MOD_ZRMS_SPH_SPEC)     >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_TRANSFORM_DIR)' $(SOURCES)
