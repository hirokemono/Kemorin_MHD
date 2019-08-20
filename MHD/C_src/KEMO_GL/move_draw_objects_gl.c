
/* move_draw_objects_gl.c */

#include <OpenGL/gl3.h>
#include "move_draw_objects_gl.h"

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage){
    glReadBuffer(GL_FRONT);
    glPixelStorei(GL_PACK_ALIGNMENT, IONE);
    glReadPixels(IZERO, IZERO, (GLsizei) num_x, (GLsizei) num_y,
                 GL_RGB, GL_UNSIGNED_BYTE,(GLubyte *) glimage);
}

void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO, 
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids **psf_solid_VAO, struct VAO_ids **psf_trans_VAO, 
			struct VAO_ids **grid_VAO, struct kemoview_shaders *kemo_shaders){
	glDeleteLists(view_s->gl_drawID, 1);
	glNewList(view_s->gl_drawID, GL_COMPILE_AND_EXECUTE);
	
	glEndList();
	
	return;
}


void draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO, 
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids **psf_solid_VAO, struct VAO_ids **psf_trans_VAO, 
			struct VAO_ids **grid_VAO, struct kemoview_shaders *kemo_shaders){
	int i;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	update_projection_struct(view_s);
	rotate_view_by_struct(view_s);
//	modify_view_by_struct(view_s);
	
	if(mesh_m->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = check_draw_map(psf_a);
	 	draw_map_objects_VAO(mesh_m, view_s, psf_solid_VAO, grid_VAO, kemo_shaders);
	} else {
		draw_axis_VAO(view_s, grid_VAO[2], kemo_shaders);
		draw_fieldlines_VAO(fline_m, view_s, grid_VAO[2], kemo_shaders);
		
		 iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		 iflag_psf = iflag_psf + check_draw_psf(psf_a);
		 draw_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, view_s, 
									psf_solid_VAO, kemo_shaders);
		
		if(mesh_m->iflag_draw_mesh != 0){
			draw_solid_mesh_VAO(mesh_m, view_s, mesh_solid_VAO, 
						mesh_grid_VAO, mesh_node_VAO, kemo_shaders);
		};

		draw_coastline_grid_VBO(view_s, grid_VAO, kemo_shaders);
		
		draw_PSF_trans_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, 
								   view_s, psf_trans_VAO, kemo_shaders);
		if(mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(mesh_s, mesh_m, view_s, mesh_trans_VAO);
			draw_trans_mesh_VAO(mesh_m, view_s, mesh_trans_VAO, kemo_shaders);
		};
	};
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	draw_colorbar_VAO(psf_a->cbar_wk, &grid_VAO[3], kemo_shaders);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		draw_initial_cube(view_s, cube_VAO, kemo_shaders);
	}
	return;
}

void update_draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO, 
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids **psf_solid_VAO, struct VAO_ids **psf_trans_VAO, 
			struct VAO_ids **grid_VAO, struct kemoview_shaders *kemo_shaders){
	int i;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
		
	
	if(mesh_m->iflag_view_type == VIEW_MAP) {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = check_draw_map(psf_a);
		set_map_objects_VAO(view_s->iflag_retina, psf_s, mesh_m, psf_m, psf_a, 
							 psf_solid_VAO, grid_VAO);
		draw_map_objects_VAO(mesh_m, view_s, psf_solid_VAO, grid_VAO, kemo_shaders);
	} else {
		glGenVertexArrays(1, &grid_VAO[2]->id_VAO);
		set_axis_VAO(mesh_m, view_s, grid_VAO[2]);
		draw_axis_VAO(view_s, grid_VAO[2], kemo_shaders);
		
		sel_fieldlines_VAO(fline_s, fline_m, cube_VAO);
		draw_fieldlines_VAO(fline_m, view_s, cube_VAO, kemo_shaders);
		
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		iflag_psf = iflag_psf + check_draw_psf(psf_a);
		set_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a,
								  psf_solid_VAO);
		draw_PSF_solid_objects_VAO(mesh_m->shading_mode, psf_s, psf_m, psf_a, view_s,
								  psf_solid_VAO, kemo_shaders);
		
	
		if(mesh_m->iflag_draw_mesh != 0){
			set_solid_mesh_VAO(mesh_s, mesh_m, mesh_solid_VAO, 
						mesh_grid_VAO, mesh_node_VAO);
			draw_solid_mesh_VAO(mesh_m, view_s, mesh_solid_VAO, 
						mesh_grid_VAO, mesh_node_VAO, kemo_shaders);
		};
		
		set_coastline_grid_VBO(mesh_m, grid_VAO);
		draw_coastline_grid_VBO(view_s, grid_VAO, kemo_shaders);
		
		/* Draw Transparent Objects */
		draw_PSF_trans_objects_VAO(mesh_m->shading_mode, 
					psf_s, psf_m, psf_a, view_s, psf_trans_VAO, kemo_shaders);
		
		if(mesh_m->iflag_draw_mesh != 0){
			set_trans_mesh_VAO(mesh_s, mesh_m, view_s, mesh_trans_VAO);
			draw_trans_mesh_VAO(mesh_m, view_s, mesh_trans_VAO, kemo_shaders);
		};
	};
	
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	
	set_colorbar_VAO(view_s->iflag_retina, view_s->nx_window, view_s->ny_window,
				mesh_m->text_color, mesh_m->bg_color, psf_m, psf_a, &grid_VAO[3]);
	draw_colorbar_VAO(psf_a->cbar_wk, &grid_VAO[3], kemo_shaders);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		set_initial_cube_VAO(view_s, cube_VAO);
		draw_initial_cube(view_s, cube_VAO, kemo_shaders);
	}
	
	return;
}
