#
#
#

OBJ_KEMO_GL_C = \
kemoviewer_param_c.o \
kemoviewer_base.o \
m_phong_light_table_c.o \
m_gl_transfer_matrix.o \
m_kemoview_mesh_menu.o \
m_kemoview_psf_menu.o \
m_kemoviewer_menu.o \
m_kemoview_psf.o \
m_kemoview_fline.o \
m_kemoview_mesh.o \
m_kemoviewer_data.o \
m_kemoviewer_structure.o \
\
write_modelview_matrix.o \
vartex_array_object_gl.o \
\
psf_data_array_manager.o \
read_data_4_kemoviewer.o \
set_kemoviewer_ucd_data.o \
select_read_psf_viewer_c.o \
\
icosahedron_c.o \
init_gl_lighting_c.o \
modify_object_4_viewer_c.o \
find_isoline_on_patch_c.o \
trackball.o \
rotate_animation.o \
rainbow_color_code_c.o \
set_color_code_on_nodes.o \
\
set_cube_to_buf.o \
set_colorbar_to_buffer.o \
set_mesh_patch_2_gl_buf.o \
set_mesh_node_2_gl_buf.o \
set_mesh_grid_2_gl_buf.o \
set_PSF_patches_to_buf.o \
set_PSF_isolines_to_buf.o \
set_map_isolines_to_buf.o \
set_fieldline_to_buf.o \
set_axis_to_buf.o \
set_coastline_to_buf.o \
set_menu_bottun_to_buf.o \
\
drawcube_gl.o \
draw_patch_4_mesh_c.o \
draw_fieldlines.o \
draw_patches_4_PSF.o \
draw_map_4_PSF.o \
draw_coastline.o \
draw_colorbar_gl.o \
draw_menu_bottun_gl.o \
\
move_draw_objects_gl.o \
modify_stereo_view_gl.o \
\
gl_buffer_2_png.o \
gl_buffer_2_bmp.o \
write_gl_window_to_file.o \
\
sort_by_patch_distance.o \
\
coastline_c.o \
set_texture_4_psf.o \
set_psf_texture_by_png.o \
\
set_each_psf_parameters.o \
set_each_fline_parameters.o \
\
psf_patch_buffer.o \
\
glsl.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLDIR =   $$(C_SRCDIR)/KEMO_GL'     >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_GL_C)' >> $(MAKENAME)


mod_list:
	@echo  OBJ_KEMO_GL_C=  \\ >> $(MAKENAME)
	@echo  $(OBJ_KEMO_GL_C) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@echo \
	kemoviewer_param_c.o: \
	'$$(KEMO_C_GLDIR)'/kemoviewer_param_c.c \
	kemoviewer_param_c.h \
	kemoviewer.h \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	kemoviewer_base.o: \
	'$$(KEMO_C_GLDIR)'/kemoviewer_base.c \
	kemoviewer_base.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_phong_light_table_c.o: \
	'$$(KEMO_C_GLDIR)'/m_phong_light_table_c.c \
	m_phong_light_table_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_gl_transfer_matrix.o: \
	'$$(KEMO_C_GLDIR)'/m_gl_transfer_matrix.c \
	m_gl_transfer_matrix.h \
	trackball.o \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	write_modelview_matrix.o: \
	'$$(KEMO_C_GLDIR)'/write_modelview_matrix.c \
	write_modelview_matrix.h \
	m_gl_transfer_matrix.o \
	m_kemoviewer_menu.o \
	kemoviewer_base.o \
	kemosrc_param_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	gl_buffer_2_png.o: \
	'$$(KEMO_C_GLDIR)'/gl_buffer_2_png.c \
	gl_buffer_2_png.h \
	kemoviewer_param_c.o \
	kemosrc_param_c.o \
	write_image_2_png.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	gl_buffer_2_bmp.o: \
	'$$(KEMO_C_GLDIR)'/gl_buffer_2_bmp.c \
	gl_buffer_2_bmp.h \
	kemoviewer_param_c.o \
	write_image_2_bmp.o \
    set_texture_4_psf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	modify_object_4_viewer_c.o: \
	'$$(KEMO_C_GLDIR)'/modify_object_4_viewer_c.c \
	modify_object_4_viewer_c.h \
	m_surface_mesh_4_viewer_c.o \
	m_psf_data_4_viewer_c.o \
	m_kemoview_psf_menu.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	kemoviewer_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	modify_stereo_view_gl.o: \
	'$$(KEMO_C_GLDIR)'/modify_stereo_view_gl.c \
	modify_stereo_view_gl.h \
	kemoviewer_param_c.o \
	m_gl_transfer_matrix.o \
	move_draw_objects_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	icosahedron_c.o: \
	'$$(KEMO_C_GLDIR)'/icosahedron_c.c \
	icosahedron_c.h \
	kemoviewer_param_c.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	init_gl_lighting_c.o: \
	'$$(KEMO_C_GLDIR)'/init_gl_lighting_c.c \
	kemoviewer_param_c.h \
	m_gl_transfer_matrix.o \
	m_kemoview_mesh_menu.o \
	glsl.o \
	m_phong_light_table_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	write_gl_window_to_file.o: \
	'$$(KEMO_C_GLDIR)'/write_gl_window_to_file.c \
	write_gl_window_to_file.h \
	move_draw_objects_gl.o \
	m_kemoviewer_menu.o \
	gl_buffer_2_png.o \
	gl_buffer_2_bmp.o \
	m_color_table_c.o \
	kemosrc_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	rainbow_color_code_c.o: \
	'$$(KEMO_C_GLDIR)'/rainbow_color_code_c.c \
	rainbow_color_code_c.h \
	set_rgba_table_c.o \
	set_rgb_colors_c.o \
	m_color_table_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	find_isoline_on_patch_c.o: \
	'$$(KEMO_C_GLDIR)'/find_isoline_on_patch_c.c \
	kemoviewer_param_c.h \
	cal_surface_center_normal_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	trackball.o: \
	'$$(KEMO_C_GLDIR)'/trackball.c \
	trackball.h \
	kemoviewer_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	rotate_animation.o: \
	'$$(KEMO_C_GLDIR)'/rotate_animation.c \
	kemoviewer_param_c.o \
	m_gl_transfer_matrix.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoviewer_data.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoviewer_data.c \
	m_kemoviewer_data.h \
	kemoviewer.h \
	kemoviewer_base.o \
	m_kemoview_psf.o \
	m_kemoview_fline.o \
	m_kemoview_mesh.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	shaders.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoviewer_structure.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoviewer_structure.c \
	m_kemoviewer_structure.h \
	kemoviewer.h \
	kemoviewer_base.o \
	m_kemoview_psf.o \
	m_kemoview_fline.o \
	m_kemoview_mesh.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	shaders.o \
	set_rgba_table_c.o \
	skip_comment_c.o \
	read_data_4_kemoviewer.o \
	write_gl_window_to_file.o \
	psf_data_array_manager.o \
	set_kemoviewer_ucd_data.o \
	write_modelview_matrix.o \
	draw_menu_bottun_gl.o \
	rotate_animation.o \
	set_each_psf_parameters.o \
	set_each_fline_parameters.o \
	rainbow_color_code_c.o \
	set_psf_texture_by_png.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoview_mesh_menu.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoview_mesh_menu.c \
	m_kemoview_mesh_menu.h \
	kemoviewer.h \
	kemosrc_param_c.o \
	kemoviewer_param_c.o \
	kemoviewer_base.o \
	m_surface_mesh_4_viewer_c.o \
	m_color_table_c.o \
	m_colorbar_work.o \
    set_rgba_table_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoview_psf_menu.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoview_psf_menu.c \
	m_kemoview_psf_menu.h \
	kemoviewer.h \
	kemosrc_param_c.o \
	kemoviewer_param_c.o \
	kemoviewer_base.o \
	m_psf_data_4_viewer_c.o \
	m_color_table_c.o \
	m_colorbar_work.o \
    set_rgba_table_c.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoviewer_menu.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoviewer_menu.c \
	kemoviewer.h \
	m_kemoviewer_menu.h \
	kemosrc_param_c.o \
	kemoviewer_base.o \
	kemoviewer_param_c.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_color_table_c.o \
	m_colorbar_work.o \
    set_rgba_table_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoview_mesh.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoview_mesh.c \
	m_kemoview_mesh.h \
	kemoviewer.h \
	kemoviewer_base.o \
	m_kemoview_mesh_menu.o \
	m_surface_mesh_4_viewer_c.o \
	read_data_4_kemoviewer.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoview_psf.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoview_psf.c \
	m_kemoview_psf.h \
	kemoviewer.h \
	kemoviewer_base.o \
	m_kemoview_psf_menu.o \
	m_psf_data_4_viewer_c.o \
	read_data_4_kemoviewer.o \
	psf_data_array_manager.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoview_fline.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoview_fline.c \
	m_kemoview_fline.h \
	kemoviewer.h \
	kemoviewer_base.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	read_data_4_kemoviewer.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_cube_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_cube_to_buf.c \
	set_cube_to_buf.h \
	kemoviewer_param_c.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_colorbar_to_buffer.o: \
	'$$(KEMO_C_GLDIR)'/set_colorbar_to_buffer.c \
	set_colorbar_to_buffer.h \
	m_colorbar_work.o \
	m_color_table_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_mesh_patch_2_gl_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_mesh_patch_2_gl_buf.c \
	set_mesh_patch_2_gl_buf.h \
	kemoviewer_param_c.o \
	m_kemoview_mesh_menu.o \
	m_surface_mesh_4_viewer_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_mesh_node_2_gl_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_mesh_node_2_gl_buf.c \
	set_mesh_node_2_gl_buf.h \
	kemoviewer_param_c.o \
	m_kemoview_mesh_menu.o \
	m_surface_mesh_4_viewer_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_mesh_grid_2_gl_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_mesh_grid_2_gl_buf.c \
	set_mesh_grid_2_gl_buf.h \
	kemoviewer_param_c.o \
	m_kemoview_mesh_menu.o \
	m_surface_mesh_4_viewer_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_PSF_patches_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_PSF_patches_to_buf.c \
	set_PSF_patches_to_buf.h \
	kemoviewer_param_c.o \
	m_kemoview_psf_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	coordinate_converter_c.o \
	set_color_code_on_nodes.o \
	icosahedron_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_PSF_isolines_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_PSF_isolines_to_buf.c \
	set_PSF_isolines_to_buf.h \
	kemoviewer_param_c.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	find_isoline_on_patch_c.o \
	set_color_code_on_nodes.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_map_isolines_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_map_isolines_to_buf.c \
	set_map_isolines_to_buf.h \
	kemoviewer_param_c.o \
	m_kemoview_psf_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	find_isoline_on_patch_c.o \
	set_color_code_on_nodes.o \
	rainbow_color_code_c.o \
	coordinate_converter_c.o \
	set_PSF_isolines_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_fieldline_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_fieldline_to_buf.c \
	set_fieldline_to_buf.h \
	kemoviewer_param_c.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	set_color_code_on_nodes.o \
	rainbow_color_code_c.o \
	icosahedron_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_axis_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_axis_to_buf.c \
	set_axis_to_buf.h \
	kemoviewer_param_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	icosahedron_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_coastline_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_coastline_to_buf.c \
	set_coastline_to_buf.h \
	kemoviewer_param_c.o \
	m_kemoviewer_menu.o \
	vartex_array_object_gl.o \
	coordinate_converter_c.o \
	rainbow_color_code_c.o \
	coastline_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_menu_bottun_to_buf.o: \
	'$$(KEMO_C_GLDIR)'/set_menu_bottun_to_buf.c \
	set_menu_bottun_to_buf.h \
	ysglfontdata.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_patch_4_mesh_c.o: \
	'$$(KEMO_C_GLDIR)'/draw_patch_4_mesh_c.c \
	kemoviewer_param_c.h \
	m_surface_mesh_4_viewer_c.o \
	m_kemoview_mesh_menu.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	sort_by_patch_distance.o \
	rainbow_color_code_c.o \
	set_mesh_patch_2_gl_buf.o \
	set_mesh_grid_2_gl_buf.o \
	set_mesh_node_2_gl_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_fieldlines.o: \
	'$$(KEMO_C_GLDIR)'/draw_fieldlines.c \
	draw_fieldlines.h \
	m_kemoviewer_menu.o \
	rainbow_color_code_c.o \
	set_color_code_on_nodes.o \
	set_fieldline_to_buf.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_patches_4_PSF.o: \
	'$$(KEMO_C_GLDIR)'/draw_patches_4_PSF.c \
	draw_patches_4_PSF.h \
	m_kemoview_psf_menu.o \
	m_psf_data_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	set_new_patch_4_map_c.o \
	coordinate_converter_c.o \
	set_color_code_on_nodes.o \
	find_isoline_on_patch_c.o \
	rainbow_color_code_c.o \
	icosahedron_c.o \
	set_PSF_patches_to_buf.o \
	set_PSF_isolines_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_map_4_PSF.o: \
	'$$(KEMO_C_GLDIR)'/draw_map_4_PSF.c \
	draw_map_4_PSF.h \
	m_kemoview_mesh_menu.o \
	m_kemoview_psf_menu.o \
	icosahedron_c.o \
	m_psf_data_4_viewer_c.o \
	set_new_patch_4_map_c.o \
	coordinate_converter_c.o \
	set_color_code_on_nodes.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	draw_coastline.o \
	rainbow_color_code_c.o \
	set_PSF_patches_to_buf.o \
	set_map_isolines_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	move_draw_objects_gl.o: \
	'$$(KEMO_C_GLDIR)'/move_draw_objects_gl.c \
	move_draw_objects_gl.h \
	kemoviewer.h \
	m_kemoviewer_data.o \
	vartex_array_object_gl.o \
	glsl.o \
	draw_colorbar_gl.o \
	draw_fieldlines.o \
	draw_coastline.o \
	drawcube_gl.o \
	draw_patches_4_PSF.o \
	draw_map_4_PSF.o \
	draw_patch_4_mesh_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_data_4_kemoviewer.o: \
	'$$(KEMO_C_GLDIR)'/read_data_4_kemoviewer.c \
	read_data_4_kemoviewer.h \
	kemoviewer_param_c.o \
	kemoviewer_base.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	m_kemoview_mesh_menu.o \
	m_kemoview_psf_menu.o \
	m_kemoviewer_menu.o \
	take_normal_psf_c.o \
	check_psf_data_viewer_c.o \
	read_viewer_mesh_c.o \
	read_viewer_mesh_gz_c.o \
	skip_comment_c.o \
	icosahedron_c.o \
	init_gl_lighting_c.o \
	set_coastline_to_buf.o \
	set_psf_viewer.o \
	set_rgba_table_c.o \
	set_surface_mesh_data.o \
	set_normal_on_node_4_mesh.o \
	take_normal_surf_mesh_c.o \
	select_read_psf_viewer_c.o \
	modify_object_4_viewer_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_kemoviewer_ucd_data.o: \
	'$$(KEMO_C_GLDIR)'/set_kemoviewer_ucd_data.c \
	set_kemoviewer_ucd_data.h \
	kemoviewer.h \
	m_kemoview_psf.o \
	m_kemoview_fline.o \
	m_kemoview_mesh.o \
	m_psf_data_4_viewer_c.o \
	m_kemoview_mesh_menu.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	kemoviewer_base.o \
	read_data_4_kemoviewer.o \
	psf_data_array_manager.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	select_read_psf_viewer_c.o: \
	'$$(KEMO_C_GLDIR)'/select_read_psf_viewer_c.c \
	select_read_psf_viewer_c.h \
	m_psf_data_4_viewer_c.o \
	m_kemoviewer_menu.o \
	read_psf_data_viewer_c.o \
	read_psf_vtk_data_c.o \
	read_psf_data_gz_c.o \
	read_psf_vtk_data_gz_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_colorbar_gl.o: \
	'$$(KEMO_C_GLDIR)'/draw_colorbar_gl.c \
	draw_colorbar_gl.h \
	m_color_table_c.o \
	m_colorbar_work.o \
	m_kemoview_psf_menu.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	coordinate_converter_c.o \
	vartex_array_object_gl.o \
	glsl.o \
	rainbow_color_code_c.o \
	set_colorbar_to_buffer.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	vartex_array_object_gl.o: \
	'$$(KEMO_C_GLDIR)'/vartex_array_object_gl.c \
	vartex_array_object_gl.h \
	kemoviewer.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_menu_bottun_gl.o: \
	'$$(KEMO_C_GLDIR)'/draw_menu_bottun_gl.c \
	draw_menu_bottun_gl.h \
	ysglfontdata.o \
	vartex_array_object_gl.o \
	glsl.o \
	set_menu_bottun_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	sort_by_patch_distance.o: \
	'$$(KEMO_C_GLDIR)'/sort_by_patch_distance.c \
	sort_by_patch_distance.h \
	m_gl_transfer_matrix.o \
	m_psf_data_4_viewer_c.o \
	m_kemoviewer_menu.o \
	m_kemoview_psf_menu.o \
	m_surface_mesh_4_viewer_c.o \
	kemosrc_param_c.o \
	quicksort_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	psf_data_array_manager.o: \
	'$$(KEMO_C_GLDIR)'/psf_data_array_manager.c \
	psf_data_array_manager.h \
	m_kemoview_psf_menu.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	drawcube_gl.o: \
	'$$(KEMO_C_GLDIR)'/drawcube_gl.c \
	drawcube_gl.h \
	kemoviewer.h \
	kemoviewer_param_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	set_cube_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_texture_4_psf.o: \
	'$$(KEMO_C_GLDIR)'/set_texture_4_psf.c \
	set_texture_4_psf.h \
	m_kemoview_psf_menu.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_psf_texture_by_png.o: \
	'$$(KEMO_C_GLDIR)'/set_psf_texture_by_png.c \
	set_psf_texture_by_png.h \
	m_kemoview_psf_menu.h \
	read_image_2_bmp.o \
	read_image_2_png.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_coastline.o: \
	'$$(KEMO_C_GLDIR)'/draw_coastline.c \
	draw_coastline.h \
	kemoviewer_param_c.o \
	m_kemoview_mesh_menu.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	glsl.o \
	set_coastline_to_buf.o \
	set_axis_to_buf.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	coastline_c.o: \
	'$$(KEMO_C_GLDIR)'/coastline_c.c \
	coastline_c.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_color_code_on_nodes.o: \
	'$$(KEMO_C_GLDIR)'/set_color_code_on_nodes.c \
	set_color_code_on_nodes.h \
	m_psf_data_4_viewer_c.o \
	m_kemoview_psf_menu.o \
	m_kemoviewer_menu.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_each_psf_parameters.o: \
	'$$(KEMO_C_GLDIR)'/set_each_psf_parameters.c \
	set_each_psf_parameters.h \
	kemoviewer.h \
	m_psf_data_4_viewer_c.o \
	m_kemoview_psf_menu.o \
	kemoviewer_base.o \
	skip_comment_c.o \
	set_rgba_table_c.o \
	set_texture_4_psf.o \
	draw_patches_4_PSF.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_each_fline_parameters.o: \
	'$$(KEMO_C_GLDIR)'/set_each_fline_parameters.c \
	set_each_fline_parameters.h \
	kemoviewer.h \
	m_psf_data_4_viewer_c.o \
	m_kemoview_psf_menu.o \
	kemoviewer_base.o \
	skip_comment_c.o \
	set_rgba_table_c.o \
	set_texture_4_psf.o \
	draw_patches_4_PSF.o \
	read_data_4_kemoviewer.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	psf_patch_buffer.o: \
	'$$(KEMO_C_GLDIR)'/psf_patch_buffer.c \
	psf_patch_buffer.h \
	m_psf_data_4_viewer_c.o \
	m_kemoviewer_menu.o \
	coordinate_converter_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	glsl.o: \
	'$$(KEMO_C_GLDIR)'/glsl.c \
	glsl.h \
	kemoviewer.h \
	m_gl_transfer_matrix.o \
	m_phong_light_table_c.o \
	shaders.o \
	invert_small_matrix_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)

includes:
	@for sources in `ls *.h`; do \
		( \
		echo 'copy include files for ' $$sources ; \
		echo $$sources': $$(KEMO_C_GLDIR)/'$$sources \
		>> $(MAKENAME); \
		echo '	cp $$(KEMO_C_GLDIR)/'$$sources . \
		>> $(MAKENAME) \
		) \
	done
