#
#
#

OBJ_KEMO_GL_C = \
kemoviewer_param_c.o \
m_psf_data_4_viewer_c.o \
m_gl_transfer_matrix.o \
m_kemoviewer_menu.o \
m_kemoviewer_structure.o \
\
write_modelview_matrix.o \
vartex_array_object_gl.o \
\
drawcube_gl.o \
ysglusefontbitmap.o \
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
draw_axis_c.o \
rainbow_color_code_c.o \
set_color_code_on_nodes.o \
\
draw_node_by_ico_c.o \
draw_grids_4_mesh.o \
draw_patch_4_mesh_c.o \
draw_isolines_4_PSF.o \
draw_isolines_4_map.o \
draw_fieldlines.o \
draw_patches_4_PSF.o \
draw_object_kemo.o \
move_draw_objects_gl.o \
modify_stereo_view_gl.o \
\
rendereps_c.o \
gl_buffer_2_png.o \
gl_buffer_2_bmp.o \
write_gl_window_to_file.o \
\
draw_colorbar_gl.o \
draw_menu_bottun_gl.o \
\
sort_by_patch_distance.o \
\
draw_mapgrid.o \
draw_coastline.o \
coastline_c.o \
set_texture_4_psf.o \
set_psf_texture_by_png.o \
gl_buffer_2_gl2ps.o \
\
set_each_psf_parameters.o \
\
psf_patch_buffer.o \
\
glsl.o \
phong.o \
gouraud.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'KEMO_C_GLDIR =   $$(C_SRCDIR)/KEMO_GL'     >> $(MAKENAME)

lib_name:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_KEMO_GL_C)' >> $(MAKENAME)
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(OBJ_GL2PS_C)'   >> $(MAKENAME)


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
	draw_axis_c.o: \
	'$$(KEMO_C_GLDIR)'/draw_axis_c.c \
	draw_axis_c.h \
	kemoviewer_param_c.o \
	m_gl_transfer_matrix.o \
	modify_object_4_viewer_c.o \
	icosahedron_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_psf_data_4_viewer_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	modify_object_4_viewer_c.o: \
	'$$(KEMO_C_GLDIR)'/modify_object_4_viewer_c.c \
	modify_object_4_viewer_c.h \
	m_surface_mesh_4_viewer_c.o \
	m_psf_data_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	kemoviewer_param_c.o \
	move_draw_objects_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	modify_stereo_view_gl.o: \
	'$$(KEMO_C_GLDIR)'/modify_stereo_view_gl.c \
	modify_stereo_view_gl.h \
	kemoviewer_param_c.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	icosahedron_c.o: \
	'$$(KEMO_C_GLDIR)'/icosahedron_c.c \
	icosahedron_c.h \
	kemoviewer_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	init_gl_lighting_c.o: \
	'$$(KEMO_C_GLDIR)'/init_gl_lighting_c.c \
	kemoviewer_param_c.h \
	m_gl_transfer_matrix.o \
	m_kemoviewer_menu.o \
	gouraud.o \
	phong.o \
	glsl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	rendereps_c.o: \
	'$$(KEMO_C_GLDIR)'/rendereps_c.c \
	kemoviewer_param_c.h \
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
	gl_buffer_2_gl2ps.o \
	rendereps_c.o \
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
	m_kemoviewer_structure.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoviewer_structure.c \
	m_kemoviewer_structure.h \
	kemoviewer.h \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
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
	rainbow_color_code_c.o \
	gl_buffer_2_gl2ps.o \
    set_psf_texture_by_png.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	m_kemoviewer_menu.o: \
	'$$(KEMO_C_GLDIR)'/m_kemoviewer_menu.c \
	kemoviewer.h \
	m_kemoviewer_menu.h \
	kemosrc_param_c.o \
	kemoviewer_param_c.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_color_table_c.o \
    set_rgba_table_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_node_by_ico_c.o: \
	'$$(KEMO_C_GLDIR)'/draw_node_by_ico_c.c \
	kemoviewer_param_c.h \
	rainbow_color_code_c.o \
	icosahedron_c.o \
	m_surface_mesh_4_viewer_c.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_grids_4_mesh.o: \
	'$$(KEMO_C_GLDIR)'/draw_grids_4_mesh.c \
	draw_grids_4_mesh.h \
	m_surface_mesh_4_viewer_c.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_patch_4_mesh_c.o: \
	'$$(KEMO_C_GLDIR)'/draw_patch_4_mesh_c.c \
	kemoviewer_param_c.h \
	m_surface_mesh_4_viewer_c.o \
	m_kemoviewer_menu.o \
	vartex_array_object_gl.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_isolines_4_PSF.o: \
	'$$(KEMO_C_GLDIR)'/draw_isolines_4_PSF.c \
	draw_isolines_4_PSF.h \
	gl2ps.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	rainbow_color_code_c.o \
	find_isoline_on_patch_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_isolines_4_map.o: \
	'$$(KEMO_C_GLDIR)'/draw_isolines_4_map.c \
	draw_isolines_4_map.h \
	gl2ps.o \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	rainbow_color_code_c.o \
	find_isoline_on_patch_c.o \
	draw_isolines_4_PSF.o \
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
	icosahedron_c.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_patches_4_PSF.o: \
	'$$(KEMO_C_GLDIR)'/draw_patches_4_PSF.c \
	draw_patches_4_PSF.h \
	m_kemoviewer_menu.o \
	icosahedron_c.o \
	m_psf_data_4_viewer_c.o \
	vartex_array_object_gl.o \
	set_new_patch_4_map_c.o \
	coordinate_converter_c.o \
	set_color_code_on_nodes.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_object_kemo.o: \
	'$$(KEMO_C_GLDIR)'/draw_object_kemo.c \
	draw_object_kemo.h \
	draw_patches_4_PSF.o \
	draw_isolines_4_PSF.o \
	draw_isolines_4_map.o \
	draw_node_by_ico_c.o \
	draw_grids_4_mesh.o \
	draw_patch_4_mesh_c.o \
	draw_colorbar_gl.o \
	draw_mapgrid.o \
	draw_coastline.o \
	sort_by_patch_distance.o \
	init_gl_lighting_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_psf_data_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	move_draw_objects_gl.o: \
	'$$(KEMO_C_GLDIR)'/move_draw_objects_gl.c \
	move_draw_objects_gl.h \
	m_kemoviewer_menu.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	vartex_array_object_gl.o \
	draw_colorbar_gl.o \
	draw_fieldlines.o \
	draw_mapgrid.o \
	draw_coastline.o \
	drawcube_gl.o \
	draw_patches_4_PSF.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	read_data_4_kemoviewer.o: \
	'$$(KEMO_C_GLDIR)'/read_data_4_kemoviewer.c \
	read_data_4_kemoviewer.h \
	kemoviewer_param_c.o \
	m_psf_data_4_viewer_c.o \
	m_surface_mesh_4_viewer_c.o \
	m_gl_transfer_matrix.o \
	m_kemoviewer_menu.o \
	take_normal_psf_c.o \
	check_psf_data_viewer_c.o \
	read_viewer_mesh_c.o \
	read_viewer_mesh_gz_c.o \
	skip_comment_c.o \
	move_draw_objects_gl.o \
	icosahedron_c.o \
	init_gl_lighting_c.o \
	modify_object_4_viewer_c.o \
	draw_mapgrid.o \
	set_psf_viewer.o \
	set_rgba_table_c.o \
	set_surface_mesh_data.o \
	set_normal_on_node_4_mesh.o \
	take_normal_surf_mesh_c.o \
	select_read_psf_viewer_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_kemoviewer_ucd_data.o: \
	'$$(KEMO_C_GLDIR)'/set_kemoviewer_ucd_data.c \
	set_kemoviewer_ucd_data.h \
	m_psf_data_4_viewer_c.o \
	skip_comment_c.o \
	m_kemoviewer_menu.o \
	m_gl_transfer_matrix.o \
	read_data_4_kemoviewer.o \
	psf_data_array_manager.o \
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
	gl2ps.o \
	draw_colorbar_gl.h \
	m_color_table_c.o \
	coordinate_converter_c.o \
	m_gl_transfer_matrix.o \
	rainbow_color_code_c.o \
	ysglusefontbitmap.o \
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
	ysglusefontbitmap.o \
	m_gl_transfer_matrix.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	ysglusefontbitmap.o: \
	'$$(KEMO_C_GLDIR)'/ysglusefontbitmap.c \
	ysglusefontbitmap.h \
	kemoviewer_param_c.o \
	ysglfontdata.o \
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
	m_kemoviewer_menu.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	drawcube_gl.o: \
	'$$(KEMO_C_GLDIR)'/drawcube_gl.c \
	drawcube_gl.h \
	kemoviewer_param_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	gl_buffer_2_gl2ps.o: \
	'$$(KEMO_C_GLDIR)'/gl_buffer_2_gl2ps.c \
	gl_buffer_2_gl2ps.h \
	kemoviewer.h \
	gl2ps.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_texture_4_psf.o: \
	'$$(KEMO_C_GLDIR)'/set_texture_4_psf.c \
	set_texture_4_psf.h \
	m_kemoviewer_menu.h \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_psf_texture_by_png.o: \
	'$$(KEMO_C_GLDIR)'/set_psf_texture_by_png.c \
	set_psf_texture_by_png.h \
	m_kemoviewer_menu.h \
	read_image_2_bmp.o \
	read_image_2_png.o \
	skip_comment_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_mapgrid.o: \
	'$$(KEMO_C_GLDIR)'/draw_mapgrid.c \
	draw_mapgrid.h \
	gl2ps.o \
	kemoviewer_param_c.o \
	vartex_array_object_gl.o \
	coordinate_converter_c.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	draw_coastline.o: \
	'$$(KEMO_C_GLDIR)'/draw_coastline.c \
	draw_coastline.h \
	kemoviewer_param_c.o \
	vartex_array_object_gl.o \
	coastline_c.o \
	coordinate_converter_c.o \
	rainbow_color_code_c.o \
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
	m_kemoviewer_menu.o \
	rainbow_color_code_c.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	set_each_psf_parameters.o: \
	'$$(KEMO_C_GLDIR)'/set_each_psf_parameters.c \
	set_each_psf_parameters.h \
	m_psf_data_4_viewer_c.o \
	m_kemoviewer_menu.o \
	skip_comment_c.o \
	set_rgba_table_c.o \
	set_texture_4_psf.o \
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
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	phong.o: \
	'$$(KEMO_C_GLDIR)'/phong.c \
	phong.h \
	glsl.o \
	m_read_text_to_character.o \
	>> $(MAKENAME)
	@echo '	''$$(CC)' -c '$$(OPTFLAGS_GL) $$(C_INCLUDE) $$<' \
	>> $(MAKENAME)
	
	@echo \
	gouraud.o: \
	'$$(KEMO_C_GLDIR)'/gouraud.c \
	gouraud.h \
	glsl.o \
	m_read_text_to_character.o \
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
