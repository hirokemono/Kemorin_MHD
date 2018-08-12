#
#
#

KEMO_CTL_C_DIR = $$(C_SRCDIR)/CONTROLS

OBJ_KEMO_CTL_C = \
 control_elements_IO_c.o \
 control_arrays_IO_c.o \
 \
 t_control_chara_int_IO.o \
 t_control_chara_real_IO.o \
 t_control_chara2_IO.o \
 t_control_real2_IO.o \
 t_control_chara2_real_IO.o \
 t_control_chara3_IO.o \
 t_control_real3_IO.o \
 \
 all_field_names_c.o \
 \
 t_ctl_data_4_platforms_c.o \
 t_ctl_data_4_FEM_mesh_c.o \
 t_ctl_data_4_time_steps_c.o \
 t_ctl_data_mhd_evo_scheme_c.o \
 t_ctl_data_4_volume_spectr_list.o \
 t_ctl_data_4_sph_monitor_c.o \
 t_ctl_data_node_monitor_c.o \
 t_ctl_data_sph_filter_list.o \
 t_ctl_data_SGS_model_c.o \
 t_ctl_data_temp_model_c.o \
 t_ctl_data_mhd_forces_c.o \
 t_ctl_data_mhd_normalize_c.o \
 t_ctl_data_MHD_boundary_c.o \
 t_ctl_data_mhd_evolution_c.o \
 t_ctl_data_4_fields_c.o \
 \
 t_ctl_data_4_sphere_model_c.o \
 t_control_data_sph_grid_c.o \
 \
 t_control_data_4_psf_c.o \
 t_control_data_4_iso_c.o \
 t_control_data_4_fline_c.o \
 t_ctl_data_4_view_transfer_c.o \
 t_ctl_data_pvr_colormap_c.o \
 t_control_data_pvr_section_list.o \
 t_control_data_pvr_isosurf_list.o \
 t_control_data_lic_masking_list.o \
 t_control_data_LIC_c.o \
 t_control_data_4_pvr_c.o \
 t_control_data_LIC_pvr_c.o \
 t_control_data_PSF_ctl_list.o \
 t_control_data_ISO_ctl_list.o \
 t_control_data_PVR_ctl_list.o \
 t_control_data_LIC_ctl_list.o \
 t_control_data_FLINE_ctl_list.o \
 t_control_data_vizs_c.o \
 t_ctl_data_zonal_means_c.o \
 t_ctl_data_SGS_MHD_model_c.o \
 \
 t_control_data_visualizer_c.o \
 t_control_data_gen_sph_grid_c.o \
 t_SGS_MHD_control_c.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_CTL_C_DIR =  $(KEMO_CTL_C_DIR)'      >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_CTL_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_KEMO_CTL_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_CTL_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	all_field_names_c.o: \
	'$$(KEMO_CTL_C_DIR)'/all_field_names_c.c \
	all_field_names_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_CTL_C_DIR)'/all_field_names_c.c \
	>> $(MAKENAME)
	
	@echo \
	control_elements_IO_c.o: \
	'$$(KEMO_CTL_C_DIR)'/control_elements_IO_c.c \
	control_elements_IO_c.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	control_arrays_IO_c.o: \
	'$$(KEMO_CTL_C_DIR)'/control_arrays_IO_c.c \
	control_arrays_IO_c.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_chara_int_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_chara_int_IO.c \
	t_control_chara_int_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_chara_real_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_chara_real_IO.c \
	t_control_chara_real_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_chara2_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_chara2_IO.c \
	t_control_chara2_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_real2_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_real2_IO.c \
	t_control_real2_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_chara2_real_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_chara2_real_IO.c \
	t_control_chara2_real_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_chara3_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_chara3_IO.c \
	t_control_chara3_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_real3_IO.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_real3_IO.c \
	t_control_real3_IO.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_platforms_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_platforms_c.c \
	t_ctl_data_4_platforms_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_FEM_mesh_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_FEM_mesh_c.c \
	t_ctl_data_4_FEM_mesh_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_time_steps_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_time_steps_c.c \
	t_ctl_data_4_time_steps_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_evo_scheme_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_mhd_evo_scheme_c.c \
	t_ctl_data_mhd_evo_scheme_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_SGS_MHD_model_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_SGS_MHD_model_c.c \
	t_ctl_data_SGS_MHD_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	t_ctl_data_SGS_model_c.o \
	t_ctl_data_temp_model_c.o \
	t_ctl_data_mhd_forces_c.o \
	t_ctl_data_mhd_normalize_c.o \
	t_ctl_data_MHD_boundary_c.o \
	t_ctl_data_mhd_evolution_c.o \
	t_ctl_data_4_fields_c.o \
	t_ctl_data_4_time_steps_c.o \
	t_ctl_data_mhd_evo_scheme_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_SGS_MHD_control_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_SGS_MHD_control_c.c \
	t_SGS_MHD_control_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	t_ctl_data_4_platforms_c.o \
	t_ctl_data_4_sph_monitor_c.o \
	t_ctl_data_node_monitor_c.o \
	t_ctl_data_zonal_means_c.o \
	t_control_data_vizs_c.o \
	t_control_data_sph_grid_c.o \
	t_ctl_data_SGS_MHD_model_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_volume_spectr_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_volume_spectr_list.c \
	t_ctl_data_4_volume_spectr_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_sph_monitor_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_sph_monitor_c.c \
	t_ctl_data_4_sph_monitor_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_ctl_data_4_volume_spectr_list.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_node_monitor_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_node_monitor_c.c \
	t_ctl_data_node_monitor_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_real3_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_zonal_means_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_zonal_means_c.c \
	t_ctl_data_zonal_means_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_psf_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_sph_filter_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_sph_filter_list.c \
	t_ctl_data_sph_filter_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_SGS_model_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_SGS_model_c.c \
	t_ctl_data_SGS_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_ctl_data_sph_filter_list.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_temp_model_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_temp_model_c.c \
	t_ctl_data_temp_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_forces_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_mhd_forces_c.c \
	t_ctl_data_mhd_forces_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara_int_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_normalize_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_mhd_normalize_c.c \
	t_ctl_data_mhd_normalize_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara_int_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_MHD_boundary_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_MHD_boundary_c.c \
	t_ctl_data_MHD_boundary_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	t_control_chara2_real_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_evolution_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_mhd_evolution_c.c \
	t_ctl_data_mhd_evolution_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_fields_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_fields_c.c \
	t_ctl_data_4_fields_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara3_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_sphere_model_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_sphere_model_c.c \
	t_ctl_data_4_sphere_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara_int_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_sph_grid_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_sph_grid_c.c \
	t_control_data_sph_grid_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_ctl_data_4_FEM_mesh_c.o \
	t_ctl_data_4_sphere_model_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_4_psf_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_4_psf_c.c \
	t_control_data_4_psf_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara_int_IO.o \
	t_control_chara2_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_4_fline_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_4_fline_c.c \
	t_control_data_4_fline_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_real3_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_view_transfer_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_4_view_transfer_c.c \
	t_ctl_data_4_view_transfer_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara_int_IO.o \
	t_control_chara2_real_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_pvr_colormap_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_ctl_data_pvr_colormap_c.c \
	t_ctl_data_pvr_colormap_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_real2_IO.o \
	t_control_real3_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_pvr_section_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_pvr_section_list.c \
	t_control_data_pvr_section_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_psf_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_pvr_isosurf_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_pvr_isosurf_list.c \
	t_control_data_pvr_isosurf_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_lic_masking_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_lic_masking_list.c \
	t_control_data_lic_masking_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_real2_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_LIC_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_LIC_c.c \
	t_control_data_LIC_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_lic_masking_list.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_4_pvr_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_4_pvr_c.c \
	t_control_data_4_pvr_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_real2_IO.o \
	t_control_chara2_real_IO.o \
	t_control_data_4_psf_c.o \
	t_ctl_data_pvr_colormap_c.o \
	t_ctl_data_4_view_transfer_c.o \
	t_control_data_pvr_section_list.o \
	t_control_data_pvr_isosurf_list.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_LIC_pvr_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_LIC_pvr_c.c \
	t_control_data_LIC_pvr_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_psf_c.o \
	t_ctl_data_pvr_colormap_c.o \
	t_ctl_data_4_view_transfer_c.o \
	t_control_data_4_pvr_c.o \
	t_control_data_LIC_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_PSF_ctl_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_PSF_ctl_list.c \
	t_control_data_PSF_ctl_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_psf_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_ISO_ctl_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_ISO_ctl_list.c \
	t_control_data_ISO_ctl_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_iso_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_PVR_ctl_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_PVR_ctl_list.c \
	t_control_data_PVR_ctl_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_pvr_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_LIC_ctl_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_LIC_ctl_list.c \
	t_control_data_LIC_ctl_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_LIC_pvr_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_FLINE_ctl_list.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_FLINE_ctl_list.c \
	t_control_data_FLINE_ctl_list.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_4_fline_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_4_iso_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_4_iso_c.c \
	t_control_data_4_iso_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_chara2_IO.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_vizs_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_vizs_c.c \
	t_control_data_vizs_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_control_data_PSF_ctl_list.o \
	t_control_data_ISO_ctl_list.o \
	t_control_data_FLINE_ctl_list.o \
	t_control_data_PVR_ctl_list.o \
	t_control_data_LIC_ctl_list.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_visualizer_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_visualizer_c.c \
	t_control_data_visualizer_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_ctl_data_4_platforms_c.o \
	t_ctl_data_4_time_steps_c.o \
	t_control_data_vizs_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_control_data_gen_sph_grid_c.o: \
	'$$(KEMO_CTL_C_DIR)'/t_control_data_gen_sph_grid_c.c \
	t_control_data_gen_sph_grid_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	t_ctl_data_4_platforms_c.o \
	t_control_data_sph_grid_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)


includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_CTL_C_DIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_CTL_C_DIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o $(TARGET) *.mod *~
