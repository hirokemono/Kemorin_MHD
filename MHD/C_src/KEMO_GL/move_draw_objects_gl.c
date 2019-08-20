
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
	int i, iflag;
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
		draw_axis_VAO(view_s, &grid_VAO[2], kemo_shaders);
		draw_fieldlines_VAO(fline_m, view_s, &grid_VAO[2], kemo_shaders);
		
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
	
	
    /* Draw Color bar
	 struct gl_strided_buffer *cbar_buf 
	 = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	 set_buffer_address_4_patch(3*128, cbar_buf);
	 alloc_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	 set_colorbar_VAO(view_s->iflag_retina,
	 view_s->nx_window, view_s->ny_window,
	 mesh_m->text_color, mesh_m->bg_color, 
	 psf_m, psf_a, &grid_VAO[3], cbar_buf);
	 free(cbar_buf->v_buf);
	 free(cbar_buf);
	 draw_colorbar_VAO(psf_a->cbar_wk, &grid_VAO[3], kemo_shaders);
	
	/* draw example cube for empty data
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		glUseProgram(kemo_shaders->phong->programId);
		
		transfer_matrix_to_shader(kemo_shaders->phong, view_s);
		
		int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
		int id_light1Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
		int id_light2Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[1].position");
		
		int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
		int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
		int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
		int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
		
		int num_light = 2;
		GLfloat  lightposition[4] =  { 1.5, 1.5,-10.0,0.0};
		GLfloat  light2position[4] = {-1.5,-1.5,-10.0,0.0};
		GLfloat white[4] = {0.6, 0.6, 0.6, 1.0};
		GLfloat shine = 20.0;
		
		glUniform1i(id_numLight, num_light);
		glUniform4fv(id_light1Position, 1, lightposition);
		glUniform4fv(id_light2Position, 1, light2position);
		
		glUniform4fv(id_MaterialAmbient, 1, white);
		glUniform4fv(id_MaterialDiffuse, 1, white);
		glUniform4fv(id_MaterialSpecular, 1, white);
		glUniform1f(id_MaterialShiness, shine);
		
		struct gl_strided_buffer *cube_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		set_buffer_address_4_patch(8, cube_buf);
		alloc_strided_buffer(cube_buf->num_nod_buf, cube_buf->ncomp_buf, cube_buf);
		
		cube_surf_VBO(0.5f, cube_VAO, cube_buf);
		
		glBindVertexArray(cube_VAO->id_VAO);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
		glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
		
		free(cube_buf->v_buf);
		free(cube_buf);
		DestroyVBO(cube_VAO);
	}
	*/
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
	
	struct gl_strided_buffer *cbar_buf 
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, cbar_buf);
	alloc_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
			set_colorbar_VAO(view_s->iflag_retina,
								  view_s->nx_window, view_s->ny_window,
								  mesh_m->text_color, mesh_m->bg_color, 
								  psf_m, psf_a, &grid_VAO[3], cbar_buf);
	free(cbar_buf->v_buf);
	free(cbar_buf);
	draw_colorbar_VAO(psf_a->cbar_wk, &grid_VAO[3], kemo_shaders);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		glUseProgram(kemo_shaders->phong->programId);
		
		transfer_matrix_to_shader(kemo_shaders->phong, view_s);
		
		int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
		int id_light1Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
		int id_light2Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[1].position");
		
		int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
		int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
		int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
		int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
		
		int num_light = 2;
		GLfloat  lightposition[4] =  { 1.5, 1.5,-10.0,0.0};
		GLfloat  light2position[4] = {-1.5,-1.5,-10.0,0.0};
		GLfloat white[4] = {0.6, 0.6, 0.6, 1.0};
		GLfloat shine = 20.0;
		
		glUniform1i(id_numLight, num_light);
		glUniform4fv(id_light1Position, 1, lightposition);
		glUniform4fv(id_light2Position, 1, light2position);
		
		glUniform4fv(id_MaterialAmbient, 1, white);
		glUniform4fv(id_MaterialDiffuse, 1, white);
		glUniform4fv(id_MaterialSpecular, 1, white);
		glUniform1f(id_MaterialShiness, shine);
		
		struct gl_strided_buffer *cube_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		set_buffer_address_4_patch(8, cube_buf);
		alloc_strided_buffer(cube_buf->num_nod_buf, cube_buf->ncomp_buf, cube_buf);
		
		cube_surf_VBO(0.5f, cube_VAO, cube_buf);
		
		glBindVertexArray(cube_VAO->id_VAO);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
		glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
		
		free(cube_buf->v_buf);
		free(cube_buf);
		DestroyVBO(cube_VAO);
	}
	
	return;
}

void draw_cube_edge_gl3(struct view_element *view_s, 
						struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *gl_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
	
	glUseProgram(kemo_shaders->test->programId);
	
	transfer_matrix_to_shader(kemo_shaders->test, view_s);

	set_buffer_address_4_patch(8, gl_buf);
	alloc_strided_buffer(gl_buf->num_nod_buf, gl_buf->ncomp_buf, gl_buf);
	
	cube_edge_VBO(0.5f, cube_VAO, gl_buf);
	
	glBindVertexArray(cube_VAO->id_VAO);
	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, 0);
	Destroy_Simple_VAO(cube_VAO);
}

void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *quad_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
	
	glUseProgram(kemo_shaders->test->programId);
	
	identity_matrix_to_shader(kemo_shaders->test);
	
	set_buffer_address_4_patch(4, quad_buf);
	alloc_strided_buffer(quad_buf->num_nod_buf, quad_buf->ncomp_buf, quad_buf);
	
	set_quadVBO(quad_VAO, quad_buf);
	
	glBindVertexArray(quad_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, quad_VAO->id_index);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
	Destroy_Simple_VAO(quad_VAO);

	free(quad_buf->v_buf);
	free(quad_buf);
	
	return;
};
