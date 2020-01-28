
/* drawGL_by_VAO.c */


#include  "drawGL_by_VAO.h"

void drawgl_patch_with_phong(struct view_element *view_s, struct VAO_ids *VAO, 
			struct kemoview_shaders *kemo_shaders){
	if(VAO->npoint_draw <= 0) return;
	
	glDisable(GL_CULL_FACE);
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, VAO->npoint_draw);
	
	return;
}

void drawgl_lines(struct view_element *view_s, 
			struct VAO_ids *VAO, struct kemoview_shaders *kemo_shaders){
	if(VAO->npoint_draw <= 0) return;
	
	glDisable(GL_CULL_FACE);
	glUseProgram(kemo_shaders->simple->programId);
	transfer_matrix_to_shader(kemo_shaders->simple, view_s);
	glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, VAO->npoint_draw);
	return;
};


void draw_map_objects_VAO(struct view_element *view_s, 
			struct VAO_ids **map_VAO, struct kemoview_shaders *kemo_shaders){
	double xwin, ywin;
	double orthogonal[16];
	int i;
	
	if(view_s->ny_frame > view_s->nx_frame) {
		xwin = 2.05;
		ywin = 2.05 * (double)view_s->ny_frame / (double)view_s->nx_frame;
	} else{
		xwin = 1.7 * (double)view_s->nx_frame / (double)view_s->ny_frame;
		ywin = 1.7;
	}
	
	orthogonal_glmat_c(-xwin, xwin, -ywin, ywin, -1.0, 1.0, orthogonal);
	
	
	/* set shading mode */
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glUseProgram(kemo_shaders->simple->programId);
	map_matrix_to_shader(kemo_shaders->simple, orthogonal);
		
	for(i=0;i<2;i++){
		if(map_VAO[i]->npoint_draw > 0){
			glBindVertexArray(map_VAO[i]->id_VAO);
			glDrawArrays(GL_TRIANGLES, IZERO, map_VAO[i]->npoint_draw);
		};
	};	
	for(i=2;i<4;i++){
		if(map_VAO[i]->npoint_draw > 0){
			glBindVertexArray(map_VAO[i]->id_VAO);
			glDrawArrays(GL_LINES, IZERO, map_VAO[i]->npoint_draw);
		};
	};
	return;
}


void draw_solid_mesh_VAO(int polygon_mode, struct view_element *view_s, 
			struct VAO_ids *mesh_solid_VAO, struct kemoview_shaders *kemo_shaders){
	if(mesh_solid_VAO->npoint_draw <= 0) return;
	
	glEnable(GL_CULL_FACE);
	if(polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
	} else if(polygon_mode == REVERSE_POLYGON) {
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(mesh_solid_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (mesh_solid_VAO->npoint_draw));
	return;
};

void draw_trans_mesh_VAO(struct view_element *view_s, 
			struct VAO_ids *mesh_trans_VAO, struct kemoview_shaders *kemo_shaders){
	if(mesh_trans_VAO->npoint_draw <= 0) return;
	
	glDisable(GL_CULL_FACE);
	glDepthMask(GL_FALSE);
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(mesh_trans_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (mesh_trans_VAO->npoint_draw));
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	return;
}
