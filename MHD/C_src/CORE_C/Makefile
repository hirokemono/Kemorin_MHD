#
#
#

KEMO_C_DIR =     $$(C_SRCDIR)/CORE_C

OBJ_KEMO_C = \
 kemosrc_param_c.o \
 \
 delete_file_c.o \
 numbers_to_bin_c.o \
 jacobian_trilinear_c.o \
 ysglfontdata.o \
 get_ysglbitmap.o \
 write_image_2_bmp.o \
 read_image_2_bmp.o \
 \
 all_field_names_c.o \
 \
 m_surface_mesh_4_viewer_c.o \
 m_grouping_mesh_4_viewer_c.o \
 m_psf_data_4_viewer_c.o \
 m_map_data_4_viewer_c.o \
 check_viewer_mesh_c.o \
 check_psf_data_viewer_c.o \
 skip_comment_c.o \
 read_viewer_mesh_c.o \
 read_grouping_mesh_c.o \
 read_psf_data_viewer_c.o \
 read_psf_vtk_data_c.o \
 set_psf_viewer.o \
 \
 control_elements_IO_c.o \
 control_arrays_IO_c.o \
 t_ctl_data_4_platforms_c.o \
 t_ctl_data_4_FEM_mesh_c.o \
 t_ctl_data_4_time_steps_c.o \
 t_ctl_data_mhd_evo_scheme_c.o \
 t_ctl_data_4_sph_monitor_c.o \
 t_ctl_data_SGS_model_c.o \
 t_ctl_data_temp_model_c.o \
 t_ctl_data_mhd_forces_c.o \
 t_SGS_MHD_control_c.o \
 \
 read_viewer_mesh_gz_c.o \
 read_psf_data_gz_c.o \
 read_psf_vtk_data_gz_c.o \
 \
 set_surface_mesh_data.o \
 set_normal_on_node_4_mesh.o \
 cal_surface_center_normal_c.o \
 take_normal_surf_mesh_c.o \
 \
 coordinate_converter_c.o \
 projection_patch_to_map.o \
 set_new_patch_4_map_c.o \
 set_map_data_4_viewer_c.o \
 take_normal_psf_c.o \
 \
 m_color_table_c.o \
 set_rgb_colors_c.o \
 set_rgba_table_c.o \
 transfer_matvec.o \
 quicksort_c.o

ifdef ZLIB_LIBS
  OBJ_KEMO_C += \
 kemo_zlib_io_c.o
endif

ifdef PNG_LIBS
  OBJ_KEMO_C += \
   write_image_2_png.o \
   read_image_2_png.o
endif

ifdef FFTW3_C
  OBJ_KEMO_C += \
 fftw3_wrapper_kemo_c.o
endif


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_DIR =  $(KEMO_C_DIR)'      >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_KEMO_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_C)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemosrc_param_c.o: \
	'$$(KEMO_C_DIR)'/kemosrc_param_c.c \
	kemosrc_param_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_surface_mesh_4_viewer_c.o: \
	'$$(KEMO_C_DIR)'/m_surface_mesh_4_viewer_c.c \
	m_surface_mesh_4_viewer_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_grouping_mesh_4_viewer_c.o: \
	'$$(KEMO_C_DIR)'/m_grouping_mesh_4_viewer_c.c \
	m_grouping_mesh_4_viewer_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_map_data_4_viewer_c.o: \
	'$$(KEMO_C_DIR)'/m_map_data_4_viewer_c.c \
	m_map_data_4_viewer_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	jacobian_trilinear_c.o: \
	'$$(KEMO_C_DIR)'/jacobian_trilinear_c.c \
	jacobian_trilinear_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	ysglfontdata.o: \
	'$$(KEMO_C_DIR)'/ysglfontdata.c \
	ysglfontdata.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/ysglfontdata.c \
	>> $(MAKENAME)
	
	@echo \
	get_ysglbitmap.o: \
	'$$(KEMO_C_DIR)'/get_ysglbitmap.c \
	get_ysglbitmap.h \
	kemosrc_param_c.o \
	numbers_to_bin_c.o \
	ysglfontdata.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/get_ysglbitmap.c \
	>> $(MAKENAME)
	
	@echo \
	write_image_2_png.o: \
	'$$(KEMO_C_DIR)'/write_image_2_png.c \
	write_image_2_png.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/write_image_2_png.c \
	>> $(MAKENAME)
	
	@echo \
	read_image_2_png.o: \
	'$$(KEMO_C_DIR)'/read_image_2_png.c \
	read_image_2_png.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/read_image_2_png.c \
	>> $(MAKENAME)
	
	@echo \
	write_image_2_bmp.o: \
	'$$(KEMO_C_DIR)'/write_image_2_bmp.c \
	write_image_2_bmp.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/write_image_2_bmp.c \
	>> $(MAKENAME)
	
	@echo \
	read_image_2_bmp.o: \
	'$$(KEMO_C_DIR)'/read_image_2_bmp.c \
	read_image_2_bmp.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/read_image_2_bmp.c \
	>> $(MAKENAME)
	
	@echo \
	all_field_names_c.o: \
	'$$(KEMO_C_DIR)'/all_field_names_c.c \
	all_field_names_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/all_field_names_c.c \
	>> $(MAKENAME)
	
	@echo \
	delete_file_c.o: \
	'$$(KEMO_C_DIR)'/delete_file_c.c \
	delete_file_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/delete_file_c.c \
	>> $(MAKENAME)
	
	@echo \
	kemo_zlib_io_c.o: \
	'$$(KEMO_C_DIR)'/kemo_zlib_io_c.c \
	kemo_zlib_io_c.h \
	kemosrc_param_c.o \
	numbers_to_bin_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/kemo_zlib_io_c.c \
	>> $(MAKENAME)
	
	@echo \
	fftw3_wrapper_kemo_c.o: \
	'$$(KEMO_C_DIR)'/fftw3_wrapper_kemo_c.c \
	fftw3_wrapper_kemo_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/fftw3_wrapper_kemo_c.c \
	>> $(MAKENAME)
	
	@echo \
	numbers_to_bin_c.o: \
	'$$(KEMO_C_DIR)'/numbers_to_bin_c.c \
	numbers_to_bin_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/numbers_to_bin_c.c \
	>> $(MAKENAME)
	
	@echo \
	coordinate_converter_c.o: \
	'$$(KEMO_C_DIR)'/coordinate_converter_c.c \
	coordinate_converter_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_psf_data_4_viewer_c.o: \
	'$$(KEMO_C_DIR)'/m_psf_data_4_viewer_c.c \
	m_psf_data_4_viewer_c.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/m_psf_data_4_viewer_c.c \
	>> $(MAKENAME)
	
	@echo \
	check_viewer_mesh_c.o: \
	'$$(KEMO_C_DIR)'/check_viewer_mesh_c.c \
	check_viewer_mesh_c.h \
	m_surface_mesh_4_viewer_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	check_psf_data_viewer_c.o: \
	'$$(KEMO_C_DIR)'/check_psf_data_viewer_c.c \
	check_psf_data_viewer_c.h \
	m_psf_data_4_viewer_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE)' \
	'$$(KEMO_C_DIR)'/check_psf_data_viewer_c.c \
	>> $(MAKENAME)
	
	@echo \
	projection_patch_to_map.o: \
	'$$(KEMO_C_DIR)'/projection_patch_to_map.c \
	projection_patch_to_map.h \
	kemosrc_param_c.o \
	coordinate_converter_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_new_patch_4_map_c.o: \
	'$$(KEMO_C_DIR)'/set_new_patch_4_map_c.c \
	set_new_patch_4_map_c.h \
	kemosrc_param_c.o \
	projection_patch_to_map.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_map_data_4_viewer_c.o: \
	'$$(KEMO_C_DIR)'/set_map_data_4_viewer_c.c \
	set_map_data_4_viewer_c.h \
	kemosrc_param_c.o \
	m_psf_data_4_viewer_c.o \
	m_map_data_4_viewer_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	take_normal_psf_c.o: \
	'$$(KEMO_C_DIR)'/take_normal_psf_c.c \
	take_normal_psf_c.h \
	m_psf_data_4_viewer_c.o \
	cal_surface_center_normal_c.o \
	coordinate_converter_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	skip_comment_c.o: \
	'$$(KEMO_C_DIR)'/skip_comment_c.c \
	skip_comment_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_grouping_mesh_c.o: \
	'$$(KEMO_C_DIR)'/read_grouping_mesh_c.c \
	read_grouping_mesh_c.h \
	m_grouping_mesh_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_psf_data_viewer_c.o: \
	'$$(KEMO_C_DIR)'/read_psf_data_viewer_c.c \
	read_psf_data_viewer_c.h \
	kemosrc_param_c.o \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_psf_vtk_data_gz_c.o: \
	'$$(KEMO_C_DIR)'/read_psf_vtk_data_gz_c.c \
	read_psf_vtk_data_gz_c.h \
	kemosrc_param_c.o \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	kemo_zlib_io_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_psf_vtk_data_c.o: \
	'$$(KEMO_C_DIR)'/read_psf_vtk_data_c.c \
	read_psf_vtk_data_c.h \
	kemosrc_param_c.o \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_viewer_mesh_c.o: \
	'$$(KEMO_C_DIR)'/read_viewer_mesh_c.c \
	read_viewer_mesh_c.h \
	m_surface_mesh_4_viewer_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_psf_viewer.o: \
	'$$(KEMO_C_DIR)'/set_psf_viewer.c \
	set_psf_viewer.h \
	kemosrc_param_c.o \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	set_new_patch_4_map_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	control_elements_IO_c.o: \
	'$$(KEMO_C_DIR)'/control_elements_IO_c.c \
	control_elements_IO_c.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	control_arrays_IO_c.o: \
	'$$(KEMO_C_DIR)'/control_arrays_IO_c.c \
	control_arrays_IO_c.h \
	kemosrc_param_c.o \
	skip_comment_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_platforms_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_4_platforms_c.c \
	t_ctl_data_4_platforms_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_FEM_mesh_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_4_FEM_mesh_c.c \
	t_ctl_data_4_FEM_mesh_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_time_steps_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_4_time_steps_c.c \
	t_ctl_data_4_time_steps_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_evo_scheme_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_mhd_evo_scheme_c.c \
	t_ctl_data_mhd_evo_scheme_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_SGS_MHD_control_c.o: \
	'$$(KEMO_C_DIR)'/t_SGS_MHD_control_c.c \
	t_SGS_MHD_control_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	t_ctl_data_4_platforms_c.o \
	t_ctl_data_4_time_steps_c.o \
	t_ctl_data_mhd_evo_scheme_c.o \
	t_ctl_data_SGS_model_c.o \
	t_ctl_data_temp_model_c.o \
	t_ctl_data_mhd_forces_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_4_sph_monitor_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_4_sph_monitor_c.c \
	t_ctl_data_4_sph_monitor_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_SGS_model_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_SGS_model_c.c \
	t_ctl_data_SGS_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_temp_model_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_temp_model_c.c \
	t_ctl_data_temp_model_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	t_ctl_data_mhd_forces_c.o: \
	'$$(KEMO_C_DIR)'/t_ctl_data_mhd_forces_c.c \
	t_ctl_data_mhd_forces_c.h \
	kemosrc_param_c.o \
	control_elements_IO_c.o \
	control_arrays_IO_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_rgb_colors_c.o: \
	'$$(KEMO_C_DIR)'/set_rgb_colors_c.c \
	set_rgb_colors_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_color_table_c.o: \
	'$$(KEMO_C_DIR)'/m_color_table_c.c \
	m_color_table_c.h \
	set_rgb_colors_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	transfer_matvec.o: \
	'$$(KEMO_C_DIR)'/transfer_matvec.c \
	transfer_matvec.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	quicksort_c.o: \
	'$$(KEMO_C_DIR)'/quicksort_c.c \
	quicksort_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_viewer_mesh_gz_c.o: \
	'$$(KEMO_C_DIR)'/read_viewer_mesh_gz_c.c \
	read_viewer_mesh_gz_c.h \
	m_surface_mesh_4_viewer_c.o \
	kemo_zlib_io_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_psf_data_gz_c.o: \
	'$$(KEMO_C_DIR)'/read_psf_data_gz_c.c \
	read_psf_data_gz_c.h \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	kemo_zlib_io_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_rgba_table_c.o: \
	'$$(KEMO_C_DIR)'/set_rgba_table_c.c \
	set_rgba_table_c.h \
	m_color_table_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_surface_mesh_data.o: \
	'$$(KEMO_C_DIR)'/set_surface_mesh_data.c \
	set_surface_mesh_data.h \
	m_surface_mesh_4_viewer_c.o \
	check_viewer_mesh_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_normal_on_node_4_mesh.o: \
	'$$(KEMO_C_DIR)'/set_normal_on_node_4_mesh.c \
	set_normal_on_node_4_mesh.h \
	m_surface_mesh_4_viewer_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	cal_surface_center_normal_c.o: \
	'$$(KEMO_C_DIR)'/cal_surface_center_normal_c.c \
	cal_surface_center_normal_c.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	take_normal_surf_mesh_c.o: \
	'$$(KEMO_C_DIR)'/take_normal_surf_mesh_c.c \
	take_normal_surf_mesh_c.h \
	kemosrc_param_c.o \
	m_surface_mesh_4_viewer_c.o \
	cal_surface_center_normal_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)


includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_C_DIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_C_DIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done

clean:
	rm -f *.o $(TARGET) *.mod *~
