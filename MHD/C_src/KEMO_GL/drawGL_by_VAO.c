
/* drawGL_by_VAO.c */


#include  "drawGL_by_VAO.h"

void drawgl_patch_with_phong(struct view_element *view_s, struct VAO_ids *VAO, 
			struct kemoview_shaders *kemo_shaders){
	if(VAO->npoint_draw <= 0) return;
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
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
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	
	glUseProgram(kemo_shaders->simple->programId);
	transfer_matrix_to_shader(kemo_shaders->simple, view_s);
	glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, VAO->npoint_draw);
	return;
};

