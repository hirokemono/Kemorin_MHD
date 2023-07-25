#
#
#

SPH_UTILS_DIR = $$(DATA_UTILS_DIR)/sph_spectr

TARGET_SPH_ENE_SPEC =      ene_sph_spec
TARGET_SPH_ENE_LAYER =     ene_sph_layer
TARGET_PICK_SPH_VOL_ENE =  pick_sph_vol_ene
TARGET_PICK_SPH_ENE =      pick_sph_ene

TARGET_APPEND_SPH_MONITORS = append_sph_monitors
TARGET_TRIM_SPH_MONITORS =   trim_sph_monitors

TARGET_PICK_SPH_SPEC =     pick_sph_spec
TARGET_PICK_GAUSS_COEF =   pick_gauss_coefs

TARGET_RENAME_SPH_FLD =    rename_spectr_field
TARGET_DIFF_SPH_SPEC =     diff_spectr_data
TARGET_ZM_SPH_SPEC =       zonal_mean_sph
TARGET_T_AVE_SPEC =        t_ave_sph_spec
TARGET_SPH_NEUTRAL =       sph_neutral_points
TARGET_TAVE_SPH_ENE_SPEC =  t_ave_sph_monitor_data
TARGET_PSUM_SPH_ENE_SPEC =  part_sum_sph_mean_square

TARGET_T_AVE_STABLE_REV =      t_ave_stable_and_reversal
TARGET_T_AVE_STABLE_REV_SPEC = t_ave_stable_and_rev_spectr

TARGET_MAX_SPH_ENE =         sph_pwr_max_mode
TARGET_SPH_ULI_LSCALE =      sph_uli_lengscale
TARGET_SPH_DYN_ELSASSER =    sph_dyn_elsasser
TARGET_FIT_DIPOLE_RATIO =    sph_fitted_dipole_ratio

TARGET_SPH_ENE_CHECK =       tests/sph_ene_check
TARGET_SPH_V_SPEC_CHECK =    tests/sph_vol_spectr_check


LIBS_SPH_UTIL =  \
'$$(LIB_VIZ)'    \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'   \
'$$(LIB_MT_STREAM)' \
'$$(LIB_FFTPACK)'   \
'$$(LIB_ISPACK)'    \
'$$(LIB_ISPACK3)'   \
'$$(LIB_NETLIB)'


LIB_FILES_SPH_UTIL =  \
'$$(LIB_VIZ_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'   \
'$$(LIB_MT_STREAM_FILE)' \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_ISPACK_FILE)'    \
'$$(LIB_ISPACK3_FILE)'   \
'$$(LIB_NETLIB_FILE)'


ifdef FORTRAN_C_CONNECT
  LIBS_SPH_UTIL+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SPH_UTIL+=   '$$(LIB_KEMO_C_FILE)'
endif

SOURCES = $(shell ls *.f90 *.F90)

MOD_SPH_UTIL_DLL = \
t_picked_sph_spectr_data_IO.o \
t_gauss_coefs_monitor_IO.o \
t_sph_volume_mean_series.o \
t_sph_volume_spectr_series.o \
t_tave_sph_volume_spectr.o \
t_tave_sph_layer_mean.o \
t_tave_sph_layer_spectr.o \
tave_sdev_sph_layer_spec.o \
tave_sdev_sph_layer_mean.o \
tave_sdev_sph_volume_spec.o \
gz_spl_sph_spectr_data_IO.o \
gz_volume_spectr_monitor_IO.o \
gz_layer_mean_monitor_IO.o \
gz_layer_spectr_monitor_IO.o \
sph_layer_monitor_snap_IO.o \
sph_volume_monitor_snap_IO.o \
old_sph_spectr_data_IO.o \
picked_sph_spectr_data_IO.o \
gz_gauss_coefs_monitor_IO.o \
count_monitor_time_series.o \
cal_tave_sph_ene_spectr.o \
m_read_sph_spectra_f.o \
m_load_picked_sph_spectr_f.o \
write_gauss_coefs_4_monitor.o \
time_average_sph_ene_spectr.o \
time_average_gauss_coefs.o \
write_snap_pick_sph_spectr.o \
time_ave_picked_sph_spectr.o \
time_ave_sph_volume_mean.o \
time_ave_sph_monitor_data.o \


MOD_MHD_CTL_DLL = \
read_mhd_control_4_c.o \
t_control_data_vizs.o \
t_ctl_data_node_monitor.o \
t_control_data_dynamo_vizs.o \
t_control_data_dynamo_sects.o \
t_ctl_SGS_3d_filter.o \
t_ctl_data_FEM_MHD.o \
t_ctl_data_FEM_MHD_control.o \
t_ctl_data_MHD.o \
t_ctl_data_MHD_model.o \
t_ctl_data_SGS_MHD.o \
t_ctl_data_SGS_filter.o \
t_ctl_data_SGS_model.o \
t_ctl_data_SPH_MHD_control.o \
t_ctl_data_coriolis_force.o \
t_ctl_data_crust_filter.o \
t_ctl_data_gravity.o \
t_ctl_data_induct_norm.o \
t_ctl_data_magnetic_scale.o \
t_ctl_data_mhd_evo_area.o \
t_ctl_data_mhd_evo_scheme.o \
t_ctl_data_mhd_evolution.o \
t_ctl_data_mhd_forces.o \
t_ctl_data_mhd_magne.o \
t_ctl_data_mhd_normalize.o \
t_ctl_data_mhd_restart.o \
t_ctl_data_momentum_norm.o \
t_ctl_data_node_boundary.o \
t_ctl_data_sph_MHD_w_psf.o \
t_ctl_data_sph_MHD_w_vizs.o \
t_ctl_data_stratified_model.o \
t_ctl_data_surf_boundary.o \
t_ctl_data_temp_model.o \
t_ctl_data_termal_norm.o \
\
ctl_data_MHD_model_IO.o \
ctl_data_SGS_MHD_model_IO.o \
ctl_data_SGS_filters_IO.o \
ctl_data_SGS_model_IO.o \
ctl_data_comp_model_IO.o \
ctl_data_mhd_evo_scheme_IO.o \
ctl_data_node_boundary_IO.o \
ctl_data_surf_boundary_IO.o \
ctl_data_temp_model_IO.o \
\
m_sph_node_group_types.o \
m_set_node_group_types.o \
t_reference_scalar_param.o \
set_surface_group_types.o


MOD_VIZ_CTL_DLL = \
t_control_data_4_pvr.o \
t_control_data_pvr_isosurfs.o \
t_control_data_pvr_sections.o \
t_control_data_pvrs.o \
t_control_data_viz3.o \
t_control_data_viz4.o \
t_control_params_4_pvr.o \
t_control_params_stereo_pvr.o \
t_ctl_data_4_projection.o \
t_ctl_data_4_screen_pixel.o \
t_ctl_data_4_streo_view.o \
t_ctl_data_4_view_transfer.o \
t_ctl_data_pvr_area.o \
t_ctl_data_pvr_colorbar.o \
t_ctl_data_pvr_colormap.o \
t_ctl_data_pvr_colormap_bar.o \
t_ctl_data_pvr_isosurface.o \
t_ctl_data_pvr_light.o \
t_ctl_data_pvr_movie.o \
t_ctl_data_pvr_section.o \
t_ctl_data_quilt_image.o \
t_ctl_data_view_transfers.o \
t_ctl_data_map_section.o \
ctl_data_each_pvr_IO.o \
ctl_data_four_vizs_IO.o \
ctl_data_pvr_colorbar_IO.o \
ctl_data_pvr_colormap_IO.o \
ctl_data_pvr_movie_IO.o \
ctl_data_three_vizs_IO.o \
ctl_data_view_transfer_IO.o \
ctl_file_each_pvr_IO.o \
ctl_file_pvr_light_IO.o \
ctl_file_pvr_modelview_IO.o \
ctl_data_map_section_IO.o \
\
viz_step_ctls_to_time_ctl.o \
t_control_data_LIC.o \
t_control_data_LIC_kernel.o \
t_control_data_LIC_noise.o \
t_control_data_LIC_pvrs.o \
ctl_data_LIC_IO.o \
ctl_data_lic_pvr_IO.o \
ctl_data_visualiser_IO.o \
ctl_file_LIC_kernel_IO.o \
ctl_file_LIC_noise_IO.o \
ctl_file_lic_pvr_IO.o \
t_control_data_4_fld_on_psf.o \
t_control_data_4_iso.o \
t_control_data_4_iso_def.o \
t_control_data_4_psf.o \
t_control_data_4_psf_def.o \
t_control_data_isosurfaces.o \
t_control_data_sections.o \
t_control_data_surfacings.o \
\
control_data_surfacing_IO.o \
ctl_data_isosurface_IO.o \
ctl_data_section_IO.o \
ctl_data_section_def_IO.o \
ctl_file_field_on_psf_IO.o \
ctl_file_isosurfaces_IO.o \
ctl_file_section_def_IO.o \
ctl_file_sections_IO.o \
ctl_data_map_rendering_IO.o \
ctl_file_map_renderings_IO.o \
t_control_data_4_map.o \
t_control_data_maps.o \
t_control_data_flines.o \
t_ctl_data_field_line.o \
ctl_data_field_line_IO.o \
ctl_file_fieldlines_IO.o \
\
t_ctl_data_const_sph_mesh.o \
t_ctl_data_gen_sph_shell.o \
t_ctl_data_volume_grouping.o \
t_ctl_data_volume_repart.o \
ctl_file_gen_sph_shell_IO.o \
ctl_data_volume_grouping_IO.o \
ctl_file_volume_repart_IO.o \
\
m_force_control_labels.o \
m_section_coef_flags.o \
pvr_surface_enhancement.o \
set_position_pvr_screen.o \
t_control_params_4_fline.o \
t_control_params_4_iso.o \
set_coefs_of_sections.o \
set_cross_section_coefs.o \
set_sections_file_ctl.o \
set_isosurface_file_ctl.o \
set_field_comp_for_viz.o \
t_psf_patch_data.o \
set_area_4_viz.o



MOD_TAVE_SPH_SPEC = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
cal_t_ave_sph_spectr_data.o \
analyzer_time_ave_sph.o \
main_time_ave_sph.o

MOD_SPH_ENE_SPEC = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
analyzer_ene_sph_shell.o \
main_ene_sph_shell.o

MOD_SPH_ENE_LAYER = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
analyzer_ene_sph_layer.o \
main_ene_sph_layer.o

MOD_PICK_SPH_VOL_ENE = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
pickup_sph_mean_square_data.o \
MPI_picked_sph_mean_sq_IO.o \
analyzer_pick_rms_vol.o \
main_pick_rms_vol.o

MOD_PICK_SPH_ENE = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
pickup_sph_mean_square_data.o \
MPI_picked_sph_mean_sq_IO.o \
analyzer_pick_rms_sph.o \
main_pick_rms_sph.o

MOD_APPEND_SPH_MONITORS = \
append_sph_monitor_files.o \
t_picked_sph_spectr_data_IO.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_data_append_sph_mntr.o \
t_gauss_coefs_monitor_IO.o \
t_ctl_param_sph_series_util.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
gz_gauss_coefs_monitor_IO.o \
write_snap_pick_sph_spectr.o \
picked_sph_spectr_data_IO.o \
t_pick_copy_monitor_data.o \
t_append_picked_spectr_file.o \
append_sph_volume_mean.o \
append_sph_volume_spectr.o \
append_sph_layer_mean.o \
append_sph_layer_spectr.o \
count_monitor_time_series.o

MOD_TRIM_SPH_MONITORS = \
trim_sph_monitor_files.o \
t_picked_sph_spectr_data_IO.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_data_append_sph_mntr.o \
t_gauss_coefs_monitor_IO.o \
t_ctl_param_sph_series_util.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
gz_gauss_coefs_monitor_IO.o \
write_snap_pick_sph_spectr.o \
picked_sph_spectr_data_IO.o \
t_pick_copy_monitor_data.o \
t_append_picked_spectr_file.o \
trim_each_sph_monitor_fiile.o \
count_monitor_time_series.o

MOD_PICK_SPH_SPEC = \
t_ctl_data_4_sph_utils.o \
input_control_sph_utils.o \
ctl_file_sph_utils_IO.o \
t_spheric_data_sph_spetr.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
analyzer_pickup_mode_sph.o \
main_pickup_mode_sph.o

MOD_PICK_GAUSS_COEF = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
analyzer_pickup_gauss_coefs.o \
main_pickup_gauss_coefs.o

MOD_RENAME_SPH_FLD =  \
t_ctl_data_sph_data_utils.o \
t_ctl_param_sph_data_utils.o \
ctl_file_sph_data_utils_IO.o \
bcast_control_sph_data_util.o \
\
analyzer_rename_sph_field.o \
main_rename_sph_field.o

MOD_DIFF_SPH_SPEC =  \
t_ctl_data_sph_data_utils.o \
t_ctl_param_sph_data_utils.o \
ctl_file_sph_data_utils_IO.o \
bcast_control_sph_data_util.o \
\
analyzer_diff_sph_spectr.o \
main_diff_sph_spectr.o

MOD_ZM_SPH_SPEC = \
t_spheric_data_sph_spetr.o \
t_ctl_data_4_sph_utils.o \
ctl_file_sph_utils_IO.o \
input_control_sph_utils.o \
m_spheric_data_sph_spetr.o \
m_ctl_params_sph_utils.o \
\
analyzer_zonal_mean_sph.o \
main_zonal_mean_sph.o

MOD_SPH_NEUTRAL = \
neutral_point_by_pick_sph.o \
t_picked_sph_spectr_data_IO.o \
count_monitor_time_series.o \
picked_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
m_spheric_data_sph_spetr.o \
t_neutral_pt_by_pick_sph.o


MOD_TAVE_SPH_ENE_SPEC = \
t_average_sph_ene_spec.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_param_sph_series_util.o \
t_picked_sph_spectr_data_IO.o \
t_sph_volume_mean_series.o \
t_sph_volume_spectr_series.o \
t_tave_sph_volume_spectr.o \
t_tave_sph_layer_mean.o \
t_tave_sph_layer_spectr.o \
tave_sdev_sph_layer_spec.o \
tave_sdev_sph_layer_mean.o \
tave_sdev_sph_volume_spec.o \
time_ave_picked_sph_spectr.o \
picked_sph_spectr_data_IO.o \
gz_spl_sph_spectr_data_IO.o \
sph_layer_monitor_snap_IO.o \
sph_volume_monitor_snap_IO.o \
old_sph_spectr_data_IO.o \
write_snap_pick_sph_spectr.o \
count_monitor_time_series.o \
cal_tave_sph_ene_spectr.o \
time_ave_sph_volume_mean.o \
time_ave_sph_monitor_data.o

MOD_MAX_SPH_ENE = \
maxmode_sph_ene_spec.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_param_sph_series_util.o \
m_maxmode_sph_ene_spectr.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
count_monitor_time_series.o

MOD_SPH_ULI_LSCALE = \
sph_uli_lengh_scale.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_param_sph_series_util.o \
m_sph_uli_lengh_scale.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
count_monitor_time_series.o

MOD_SPH_DYN_ELSASSER = \
sph_dynamic_elsasser.o \
t_ctl_data_get_dyn_elsasser.o \
t_ctl_param_sph_series_util.o \
t_ctl_param_dyn_elsasser.o \
t_dyn_elsasser_address.o \
m_sph_dynamic_elsasser.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
count_monitor_time_series.o \
cal_dyn_elsasser_by_spectr.o

MOD_FIT_DIPOLE_RATIO = \
gz_spl_sph_spectr_data_IO.o \
gz_volume_spectr_monitor_IO.o \
gz_layer_mean_monitor_IO.o \
gz_layer_spectr_monitor_IO.o \
old_sph_spectr_data_IO.o \
count_monitor_time_series.o \
t_ctl_data_dipole_fit_ratio.o \
t_ctl_param_dipole_fit.o \
m_read_sph_spectra_f.o \
sph_dipole_fitting.o

MOD_PSUM_SPH_ENE_SPEC = \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_param_sph_series_util.o \
m_part_sum_sph_ene_spectr.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
count_monitor_time_series.o \
part_sum_sph_ene_spec.o

MOD_T_AVE_STABLE_REV = \
t_ctl_data_tave_stable_rev.o \
t_gauss_coefs_monitor_IO.o \
t_ctl_param_sph_series_util.o \
t_sph_volume_mean_series.o \
count_monitor_time_series.o \
gz_gauss_coefs_monitor_IO.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
write_gauss_coefs_4_monitor.o \
cal_tave_sph_ene_spectr.o \
time_average_gauss_coefs.o \
t_ave_stable_and_reversal.o

MOD_T_AVE_STABLE_REV_SPEC = \
t_ctl_data_tave_stable_rev.o \
t_gauss_coefs_monitor_IO.o \
t_ctl_param_sph_series_util.o \
t_sph_volume_spectr_series.o \
count_monitor_time_series.o \
gz_gauss_coefs_monitor_IO.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
write_gauss_coefs_4_monitor.o \
cal_tave_sph_ene_spectr.o \
time_average_gauss_coefs.o \
t_ave_spectr_stable_and_rev.o

MOD_SPH_ENE_CHECK = \
compare_sph_mean_square.o \
old_sph_spectr_data_IO.o \
gz_spl_sph_spectr_data_IO.o

MOD_SPH_V_SPEC_CHECK = \
compare_sph_mean_spectr.o \
old_sph_spectr_data_IO.o \
gz_spl_sph_spectr_data_IO.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_UTILS_DIR = $(SPH_UTILS_DIR)'     >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_ENE_SPEC =     $$(BUILDDIR)/$(TARGET_SPH_ENE_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_ENE_LAYER =    $$(BUILDDIR)/$(TARGET_SPH_ENE_LAYER)' \
	>> $(MAKENAME)
	@echo 'TARGET_PICK_SPH_VOL_ENE = $$(BUILDDIR)/$(TARGET_PICK_SPH_VOL_ENE)' \
	>> $(MAKENAME)
	@echo 'TARGET_PICK_SPH_ENE =     $$(BUILDDIR)/$(TARGET_PICK_SPH_ENE)' \
	>> $(MAKENAME)
	@echo 'TARGET_PICK_SPH_SPEC =    $$(BUILDDIR)/$(TARGET_PICK_SPH_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_APPEND_SPH_MONITORS = $$(BUILDDIR)/$(TARGET_APPEND_SPH_MONITORS)' \
	>> $(MAKENAME)
	@echo 'TARGET_TRIM_SPH_MONITORS = $$(BUILDDIR)/$(TARGET_TRIM_SPH_MONITORS)' \
	>> $(MAKENAME)
	@echo 'TARGET_PICK_GAUSS_COEF =  $$(BUILDDIR)/$(TARGET_PICK_GAUSS_COEF)' \
	>> $(MAKENAME)
	@echo 'TARGET_RENAME_SPH_FLD =   $$(BUILDDIR)/$(TARGET_RENAME_SPH_FLD)' \
	>> $(MAKENAME)
	@echo 'TARGET_DIFF_SPH_SPEC =    $$(BUILDDIR)/$(TARGET_DIFF_SPH_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_ZM_SPH_SPEC =      $$(BUILDDIR)/$(TARGET_ZM_SPH_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_T_AVE_SPEC =       $$(BUILDDIR)/$(TARGET_T_AVE_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_NEUTRAL =      $$(BUILDDIR)/$(TARGET_SPH_NEUTRAL)' \
	>> $(MAKENAME)
	@echo 'TARGET_TAVE_SPH_ENE_SPEC = $$(BUILDDIR)/$(TARGET_TAVE_SPH_ENE_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_T_AVE_STABLE_REV = $$(BUILDDIR)/$(TARGET_T_AVE_STABLE_REV)' \
	>> $(MAKENAME)
	@echo 'TARGET_T_AVE_STABLE_REV_SPEC = $$(BUILDDIR)/$(TARGET_T_AVE_STABLE_REV_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_MAX_SPH_ENE = $$(BUILDDIR)/$(TARGET_MAX_SPH_ENE)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_ULI_LSCALE = $$(BUILDDIR)/$(TARGET_SPH_ULI_LSCALE)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_DYN_ELSASSER = $$(BUILDDIR)/$(TARGET_SPH_DYN_ELSASSER)' \
	>> $(MAKENAME)
	@echo 'TARGET_FIT_DIPOLE_RATIO = $$(BUILDDIR)/$(TARGET_FIT_DIPOLE_RATIO)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_ENE_CHECK = $$(BUILDDIR)/$(TARGET_SPH_ENE_CHECK)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_V_SPEC_CHECK = $$(BUILDDIR)/$(TARGET_SPH_V_SPEC_CHECK)' \
	>> $(MAKENAME)
	@echo 'TARGET_PSUM_SPH_ENE_SPEC = $$(BUILDDIR)/$(TARGET_PSUM_SPH_ENE_SPEC)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_data_util: \
	'$$(TARGET_SPH_ENE_SPEC)        $$(TARGET_SPH_ENE_LAYER)' \
	'$$(TARGET_PICK_SPH_VOL_ENE)    $$(TARGET_PICK_SPH_ENE)'  \
	'$$(TARGET_PICK_SPH_SPEC)       $$(TARGET_PICK_GAUSS_COEF)' \
	'$$(TARGET_RENAME_SPH_FLD)      $$(TARGET_DIFF_SPH_SPEC)'  \
	'$$(TARGET_ZM_SPH_SPEC)         $$(TARGET_T_AVE_SPEC)'      \
	'$$(TARGET_TAVE_SPH_ENE_SPEC)   $$(TARGET_SPH_NEUTRAL)'  \
	'$$(TARGET_APPEND_SPH_MONITORS) $$(TARGET_TRIM_SPH_MONITORS)'  \
	'$$(TARGET_MAX_SPH_ENE)         $$(TARGET_SPH_ULI_LSCALE)'     \
	'$$(TARGET_FIT_DIPOLE_RATIO)    $$(TARGET_SPH_ENE_CHECK)'      \
	'$$(TARGET_SPH_V_SPEC_CHECK) '      \
	'$$(TARGET_PSUM_SPH_ENE_SPEC)   $$(TARGET_SPH_DYN_ELSASSER)'   \
	'$$(TARGET_T_AVE_STABLE_REV)    $$(TARGET_T_AVE_STABLE_REV_SPEC)' >> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ENE_SPEC)': '$$(MOD_SPH_ENE_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_ENE_SPEC)' \
	'$$(MOD_SPH_ENE_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ENE_LAYER)': '$$(MOD_SPH_ENE_LAYER)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_ENE_LAYER)' \
	'$$(MOD_SPH_ENE_LAYER)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_SPH_VOL_ENE)': '$$(MOD_PICK_SPH_VOL_ENE)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_PICK_SPH_VOL_ENE)' \
	'$$(MOD_PICK_SPH_VOL_ENE)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_SPH_ENE)': '$$(MOD_PICK_SPH_ENE)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_PICK_SPH_ENE)' \
	'$$(MOD_PICK_SPH_ENE)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_APPEND_SPH_MONITORS)': '$$(MOD_APPEND_SPH_MONITORS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_APPEND_SPH_MONITORS)' \
	'$$(MOD_APPEND_SPH_MONITORS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TRIM_SPH_MONITORS)': '$$(MOD_TRIM_SPH_MONITORS)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_TRIM_SPH_MONITORS)' \
	'$$(MOD_TRIM_SPH_MONITORS)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_SPH_SPEC)': '$$(MOD_PICK_SPH_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_PICK_SPH_SPEC)' \
	'$$(MOD_PICK_SPH_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PICK_GAUSS_COEF)': '$$(MOD_PICK_GAUSS_COEF)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_PICK_GAUSS_COEF)' \
	'$$(MOD_PICK_GAUSS_COEF)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_RENAME_SPH_FLD)': '$$(MOD_RENAME_SPH_FLD)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_RENAME_SPH_FLD)' \
	'$$(MOD_RENAME_SPH_FLD)' \
	'-L. $$(LIBS_SPH_UTIL)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DIFF_SPH_SPEC)': '$$(MOD_DIFF_SPH_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_DIFF_SPH_SPEC)' \
	'$$(MOD_DIFF_SPH_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ZM_SPH_SPEC)': '$$(MOD_ZM_SPH_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_ZM_SPH_SPEC)' \
	'$$(MOD_ZM_SPH_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_T_AVE_SPEC)': '$$(MOD_TAVE_SPH_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_T_AVE_SPEC)' \
	'$$(MOD_TAVE_SPH_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_NEUTRAL)': '$$(MOD_SPH_NEUTRAL)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_NEUTRAL)' \
	'$$(MOD_SPH_NEUTRAL)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TAVE_SPH_ENE_SPEC)': '$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_TAVE_SPH_ENE_SPEC)' \
	'$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSUM_SPH_ENE_SPEC)': '$$(MOD_PSUM_SPH_ENE_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_PSUM_SPH_ENE_SPEC)' \
	'$$(MOD_PSUM_SPH_ENE_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_T_AVE_STABLE_REV)': '$$(MOD_T_AVE_STABLE_REV)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_T_AVE_STABLE_REV)' \
	'$$(MOD_T_AVE_STABLE_REV)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_T_AVE_STABLE_REV_SPEC)': '$$(MOD_T_AVE_STABLE_REV_SPEC)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_T_AVE_STABLE_REV_SPEC)' \
	'$$(MOD_T_AVE_STABLE_REV_SPEC)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAX_SPH_ENE)': '$$(MOD_MAX_SPH_ENE)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_MAX_SPH_ENE)' \
	'$$(MOD_MAX_SPH_ENE)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ULI_LSCALE)': '$$(MOD_SPH_ULI_LSCALE)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_ULI_LSCALE)' \
	'$$(MOD_SPH_ULI_LSCALE)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_DYN_ELSASSER)': '$$(MOD_SPH_DYN_ELSASSER)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_DYN_ELSASSER)' \
	'$$(MOD_SPH_DYN_ELSASSER)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FIT_DIPOLE_RATIO)': '$$(MOD_FIT_DIPOLE_RATIO)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_FIT_DIPOLE_RATIO)' \
	'$$(MOD_FIT_DIPOLE_RATIO)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ENE_CHECK)': '$$(MOD_SPH_ENE_CHECK)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_ENE_CHECK)' \
	'$$(MOD_SPH_ENE_CHECK)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_V_SPEC_CHECK)': '$$(MOD_SPH_V_SPEC_CHECK)' \
	'$$(LIB_FILES_SPH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(TARGET_SPH_V_SPEC_CHECK)' \
	'$$(MOD_SPH_V_SPEC_CHECK)' \
	'-L. $$(LIBS_SPH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_SPH_UTIL=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SPH_UTIL)            >> $(MAKENAME)
	@echo  LIB_FILES_SPH_UTIL=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SPH_UTIL)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_SPH_UTIL_DLL=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_UTIL_DLL)       >> $(MAKENAME)
	@echo  MOD_MHD_CTL_DLL=  \\      >> $(MAKENAME)
	@echo  $(MOD_MHD_CTL_DLL)        >> $(MAKENAME)
	@echo  MOD_VIZ_CTL_DLL=  \\      >> $(MAKENAME)
	@echo  $(MOD_VIZ_CTL_DLL)        >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  MOD_SPH_ENE_SPEC=  \\        >> $(MAKENAME)
	@echo  $(MOD_SPH_ENE_SPEC)          >> $(MAKENAME)
	@echo  MOD_SPH_ENE_LAYER=  \\       >> $(MAKENAME)
	@echo  $(MOD_SPH_ENE_LAYER)         >> $(MAKENAME)
	@echo  MOD_PICK_SPH_VOL_ENE=  \\    >> $(MAKENAME)
	@echo  $(MOD_PICK_SPH_VOL_ENE)      >> $(MAKENAME)
	@echo  MOD_PICK_SPH_ENE=  \\        >> $(MAKENAME)
	@echo  $(MOD_PICK_SPH_ENE)          >> $(MAKENAME)
	@echo  MOD_APPEND_SPH_MONITORS=  \\ >> $(MAKENAME)
	@echo  $(MOD_APPEND_SPH_MONITORS)   >> $(MAKENAME)
	@echo  MOD_TRIM_SPH_MONITORS=  \\   >> $(MAKENAME)
	@echo  $(MOD_TRIM_SPH_MONITORS)     >> $(MAKENAME)
	@echo  MOD_PICK_SPH_SPEC=  \\       >> $(MAKENAME)
	@echo  $(MOD_PICK_SPH_SPEC)         >> $(MAKENAME)
	@echo  MOD_PICK_GAUSS_COEF=  \\     >> $(MAKENAME)
	@echo  $(MOD_PICK_GAUSS_COEF)       >> $(MAKENAME)
	@echo  MOD_RENAME_SPH_FLD=  \\      >> $(MAKENAME)
	@echo  $(MOD_RENAME_SPH_FLD)        >> $(MAKENAME)
	@echo  MOD_DIFF_SPH_SPEC=  \\       >> $(MAKENAME)
	@echo  $(MOD_DIFF_SPH_SPEC)         >> $(MAKENAME)
	@echo  MOD_ZM_SPH_SPEC=  \\         >> $(MAKENAME)
	@echo  $(MOD_ZM_SPH_SPEC)           >> $(MAKENAME)
	@echo  MOD_TAVE_SPH_SPEC=  \\       >> $(MAKENAME)
	@echo  $(MOD_TAVE_SPH_SPEC)         >> $(MAKENAME)
	@echo  MOD_SPH_NEUTRAL=  \\         >> $(MAKENAME)
	@echo  $(MOD_SPH_NEUTRAL)           >> $(MAKENAME)
	@echo  MOD_TAVE_SPH_ENE_SPEC=  \\     >> $(MAKENAME)
	@echo  $(MOD_TAVE_SPH_ENE_SPEC)       >> $(MAKENAME)
	@echo  MOD_MAX_SPH_ENE=  \\           >> $(MAKENAME)
	@echo  $(MOD_MAX_SPH_ENE)             >> $(MAKENAME)
	@echo  MOD_SPH_ULI_LSCALE=  \\        >> $(MAKENAME)
	@echo  $(MOD_SPH_ULI_LSCALE)          >> $(MAKENAME)
	@echo  MOD_SPH_DYN_ELSASSER=  \\      >> $(MAKENAME)
	@echo  $(MOD_SPH_DYN_ELSASSER)        >> $(MAKENAME)
	@echo  MOD_FIT_DIPOLE_RATIO=  \\      >> $(MAKENAME)
	@echo  $(MOD_FIT_DIPOLE_RATIO)        >> $(MAKENAME)
	@echo  MOD_PSUM_SPH_ENE_SPEC=  \\     >> $(MAKENAME)
	@echo  $(MOD_PSUM_SPH_ENE_SPEC)       >> $(MAKENAME)
	@echo  MOD_T_AVE_NUSSELT=  \\         >> $(MAKENAME)
	@echo  $(MOD_T_AVE_NUSSELT)           >> $(MAKENAME)
	@echo  MOD_T_AVE_STABLE_REV=  \\      >> $(MAKENAME)
	@echo  $(MOD_T_AVE_STABLE_REV)        >> $(MAKENAME)
	@echo  MOD_T_AVE_STABLE_REV_SPEC=  \\ >> $(MAKENAME)
	@echo  $(MOD_T_AVE_STABLE_REV_SPEC)   >> $(MAKENAME)
	@echo  MOD_SPH_ENE_CHECK=  \\         >> $(MAKENAME)
	@echo  $(MOD_SPH_ENE_CHECK)           >> $(MAKENAME)
	@echo  MOD_SPH_V_SPEC_CHECK=  \\      >> $(MAKENAME)
	@echo  $(MOD_SPH_V_SPEC_CHECK)        >> $(MAKENAME)
	@echo   >> $(MAKENAME)

module:
	@sed -e"s/F90OPTFLAGS/F90DLLFLAGS/g" Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_UTILS_DIR)' $(SOURCES)
