#
#
#

PYTHON_SRC_DIR = $$(MHDDIR)/pythons

LIB_PYTHONS = '-L$$(PYTHONDIR) -lcalypso_to_pythons'

ifdef COCOA_FLAG
  LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.dylib'
else
  LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.so'
endif

LIB_PYTHON_UTILS =       '$$(LIB_MESH)'
LIB_FILES_PYTHON_UTILS = '$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIB_PYTHON_UTILS+=        '$$(LIB_KEMO_C)'
  LIB_FILES_PYTHON_UTILS+=  '$$(LIB_KEMO_C_FILE)'
endif

MOD_PYTHONS = \
'$$(MOD_MHD_F2003)' \
'$$(MOD_VIZ_F2003)' \
'$$(MOD_SPH_UTIL_DLL)' \
'$$(MOD_MHD_CTL_DLL)' \
'$$(MOD_VIZ_CTL_DLL)'

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
t_ctl_data_valuable_density.o \
t_ctl_data_valuable_diffuse.o \
t_ctl_data_ref_point.o \
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
ctl_data_SGS_SPH_MHD_IO.o \
\
m_sph_node_group_types.o \
m_fem_node_group_types.o \
t_reference_scalar_param.o \
set_surface_group_types.o


MOD_VIZ_CTL_DLL = \
t_control_data_4_pvr.o \
t_control_data_pvr_isosurfs.o \
t_control_data_pvr_sections.o \
t_control_data_pvrs.o \
t_control_data_viz4.o \
t_control_params_4_pvr.o \
t_control_params_stereo_pvr.o \
t_control_data_tracers.o \
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
t_ctl_data_pvr_tracer.o \
m_pvr_control_labels.o \
ctl_data_each_pvr_IO.o \
ctl_data_pvr_colorbar_IO.o \
ctl_data_pvr_colormap_IO.o \
ctl_data_pvr_movie_IO.o \
ctl_data_four_vizs_IO.o \
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
t_control_data_pvr_tracers.o \
t_ctl_data_field_line.o \
t_fline_seeds_list_ctl.o \
t_ctl_params_viz_fields.o \
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
set_area_4_viz.o \
viz4_step_ctls_to_time_ctl.o \
\
m_control_fline_flags.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PYTHON_SRC_DIR = $(PYTHON_SRC_DIR)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

target_list:

target_task:

lib_name:
	@echo  LIB_PYTHONS=  \\                >> $(MAKENAME)
	@echo  $(LIB_PYTHONS)                  >> $(MAKENAME)
	@echo  LIB_FILE_PYTHONS=  \\           >> $(MAKENAME)
	@echo  $(LIB_FILE_PYTHONS)             >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIB_PYTHON_UTILS=  \\           >> $(MAKENAME)
	@echo  $(LIB_PYTHON_UTILS)             >> $(MAKENAME)
	@echo  LIB_FILES_PYTHON_UTILS=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PYTHON_UTILS)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

lib_tasks:
	@echo '$$(LIB_FILE_PYTHONS)': '$$(MOD_PYTHONS)' \
	'$$(LIB_FILES_PYTHON_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -shared  '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(LIB_FILE_PYTHONS)' \
	'$$(MOD_PYTHONS)' \
	'-L. $$(LIB_PYTHON_UTILS)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_PYTHONS=  \\     >> $(MAKENAME)
	@echo  $(MOD_PYTHONS)       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo  MOD_SPH_UTIL_DLL=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_UTIL_DLL)       >> $(MAKENAME)
	@echo  MOD_MHD_CTL_DLL=  \\      >> $(MAKENAME)
	@echo  $(MOD_MHD_CTL_DLL)        >> $(MAKENAME)
	@echo  MOD_VIZ_CTL_DLL=  \\      >> $(MAKENAME)
	@echo  $(MOD_VIZ_CTL_DLL)        >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
