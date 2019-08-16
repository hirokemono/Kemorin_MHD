
/* move_draw_objects_gl.c */

#include <OpenGL/gl3.h>
#include "move_draw_objects_gl.h"

void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf, struct gl_strided_buffer *strided_buf,
				  struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	int i, iflag;
	int iflag_psf = 0;
	
	glDeleteLists(view_s->gl_drawID, 1);
	glNewList(view_s->gl_drawID, GL_COMPILE_AND_EXECUTE);
	
    /* Draw Solid Objects */
	
	glEnable(GL_COLOR_MATERIAL);
	if(fline_m->iflag_draw_fline != 0){
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		if(fline_m->fieldline_type == IFLAG_PIPE){
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glDisable(GL_CULL_FACE);
			glShadeModel(GL_SMOOTH);
			
			draw_fieldtubes_c(fline_s, fline_m, gl_buf);
			glEnable(GL_CULL_FACE);
		} else {
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			
			draw_fieldlines_c(fline_s, fline_m, gl_buf);
		};
	};
    
    if(mesh_m->iflag_view_type == VIEW_MAP) {
        set_color_code_for_psfs(psf_s, psf_m, psf_a);
        iflag_psf = draw_objects_4_map(psf_s, mesh_m, psf_m, psf_a, view_s, gl_buf);
    } else {
		iflag_psf = iflag_psf + iflag;
	};
    
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		if(mesh_m->iflag_draw_sph_grid != 0){draw_sph_flame(mesh_m->radius_coast, gl_buf);};
	};
	
    /* Draw Color bar */
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
		if(psf_a->iflag_loaded[i] != 0){
			if(psf_m[i]->draw_psf_cbar > 0) {
				draw_colorbar_gl(view_s->iflag_retina,
                                 view_s->nx_window, view_s->ny_window,
                                 mesh_m->text_color, mesh_m->bg_color, psf_m[i]->cmap_psf);
				load_projection_matrix(view_s);
				rotate_view_by_struct(view_s);
			};
		};
	};
	
	glEndList();
	
	return;
}


static int draw_PSF_solid_objects_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
    int i;
    int iflag_psf = 0;
    
    for(i=0; i<psf_a->nmax_loaded; i++){
        iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
        if(psf_a->iflag_loaded[i] != 0){
			
			if(psf_m[i]->draw_psf_vect  != 0){
				draw_PSF_arrow_VAO(psf_s[i], psf_m[i], view_s, 
							psf_VAO, kemo_shaders, psf_buf);
			};
			if( (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero) != 0){
				draw_PSF_isoline_VAO(psf_s[i], psf_m[i], view_s, 
							psf_VAO, kemo_shaders, psf_buf);
			};
		};
	};
	
	return iflag_psf;
}

void draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf, struct gl_strided_buffer *strided_buf,
				  struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	int i, iflag;
	int iflag_psf = 0;
	
    /* Draw Solid Objects */
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
		
	if(mesh_m->iflag_draw_axis != 0){
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		glDisable(GL_CULL_FACE);
		struct gl_strided_buffer *axis_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		draw_axis_VAO(view_s, (GLfloat) mesh_m->dist_domains, cube_VAO, kemo_shaders, axis_buf);
	}
	
	
	/*
	if(fline_m->iflag_draw_fline != 0){
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		if(fline_m->fieldline_type == IFLAG_PIPE){
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glDisable(GL_CULL_FACE);
			glShadeModel(GL_SMOOTH);
			
			draw_fieldtubes_c(fline_s, fline_m, gl_buf);
			glEnable(GL_CULL_FACE);
		} else {
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			
			draw_fieldlines_c(fline_s, fline_m, gl_buf);
		};
	};
    */
	struct gl_strided_buffer *psf_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, psf_buf);
	alloc_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	if(mesh_m->iflag_view_type == VIEW_MAP) {
		set_color_code_for_psfs(psf_s, psf_m, psf_a);
		iflag_psf = draw_map_objects_VAO(psf_s, mesh_m, psf_m, psf_a, view_s, 
					cube_VAO, kemo_shaders, psf_buf, gl_buf);
	} else {
		iflag_psf = sort_by_patch_distance_psfs(psf_s, psf_m, psf_a, view_s);
		set_color_code_for_psfs(psf_s, psf_m, psf_a);
		
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		draw_PSF_texture_VAO(mesh_m->shading_mode, IZERO, psf_a->istack_solid_psf_txtur, 
					psf_s, psf_m, psf_a, view_s, cube_VAO, kemo_shaders->phong_texure, psf_buf);
		draw_PSF_patch_VAO(mesh_m->shading_mode, psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch, 
					psf_s, psf_m, psf_a, view_s, cube_VAO, kemo_shaders, psf_buf);
		
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		iflag = draw_PSF_solid_objects_VAO(psf_s, psf_m, psf_a, view_s,
					cube_VAO, kemo_shaders, psf_buf);
		iflag_psf = iflag_psf + iflag;
	};
	free(psf_buf->v_buf);
	free(psf_buf);
	
	
	if(mesh_m->iflag_draw_mesh != 0){
		
		struct gl_strided_buffer *mesh_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		
		glDisable(GL_CULL_FACE);
		draw_mesh_edges_VAO(mesh_s, mesh_m, view_s, cube_VAO, kemo_shaders, mesh_buf);
		
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT, GL_FILL);
		draw_nodes_ico_VAO(mesh_s, mesh_m, view_s, cube_VAO, kemo_shaders, mesh_buf);
		
		glEnable(GL_CULL_FACE);
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
		draw_mesh_patches_VAO(mesh_s, mesh_m, view_s, cube_VAO, kemo_shaders, mesh_buf);
		free(mesh_buf);
		DestroyVBO(cube_VAO);
	};
	
	
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		struct gl_strided_buffer *line_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		set_buffer_address_4_patch(3*128, line_buf);
		alloc_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
		
		if(mesh_m->iflag_draw_coast != 0){
			draw_coastline_VBO(mesh_m->radius_coast, view_s, cube_VAO, kemo_shaders, line_buf);
		};
/*		if(mesh_m->iflag_draw_sph_grid != 0){draw_sph_flame(mesh_m->radius_coast, gl_buf);};*/
		free(line_buf->v_buf);
		free(line_buf);
	};
	
    /* Draw Transparent Objects */
	
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDepthMask(GL_FALSE);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		
		/* set shading mode */
		struct gl_strided_buffer *psf_buf2 = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		set_buffer_address_4_patch(3*128, psf_buf2);
		alloc_strided_buffer(psf_buf2->num_nod_buf, psf_buf2->ncomp_buf, psf_buf2);
		
		glDisable(GL_CULL_FACE);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		draw_PSF_texture_VAO(mesh_m->shading_mode, 
					psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur, 
					psf_s, psf_m, psf_a, view_s, cube_VAO, kemo_shaders->phong_texure, psf_buf2);
		draw_PSF_patch_VAO(mesh_m->shading_mode, 
					psf_a->istack_trans_psf_txtur, psf_a->ntot_psf_patch,
					psf_s, psf_m, psf_a, view_s, cube_VAO, kemo_shaders, psf_buf2);
		free(psf_buf2->v_buf);
		free(psf_buf2);
	};
	
	if(mesh_m->iflag_draw_mesh != 0){
		if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
			glPolygonMode(GL_FRONT, GL_FILL);
			glCullFace(GL_BACK);
		}
		else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
			glPolygonMode(GL_BACK, GL_FILL);
			glCullFace(GL_FRONT);
		};
		
		struct gl_strided_buffer *mesh_buf2 = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
		draw_transparent_mesh_VAO(mesh_s, mesh_m, view_s, cube_VAO, kemo_shaders, mesh_buf2);
		free(mesh_buf2);
		DestroyVBO(cube_VAO);
	};
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	
	
    /* Draw Color bar
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
		if(psf_a->iflag_loaded[i] != 0){
			if(psf_m[i]->draw_psf_cbar > 0) {
				draw_colorbar_gl(view_s->iflag_retina,
                                 view_s->nx_window, view_s->ny_window,
                                 mesh_m->text_color, mesh_m->bg_color, psf_m[i]->cmap_psf);
				load_projection_matrix(view_s);
				rotate_view_by_struct(view_s);
			};
		};
	};
	
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glDisable(GL_COLOR_MATERIAL);
	
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
	
	GLuint idx_indexBuf;
	
	set_buffer_address_4_patch(8, gl_buf);
	alloc_strided_buffer(gl_buf->num_nod_buf, gl_buf->ncomp_buf, gl_buf);
	
	cube_edge_VBO(0.5f, cube_VAO, gl_buf);
	
	glBindVertexArray(cube_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, 0);
	
	DestroyVBO(cube_VAO);
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
	DestroyVBO(quad_VAO);

	free(quad_buf->v_buf);
	free(quad_buf);
	
	return;
};
