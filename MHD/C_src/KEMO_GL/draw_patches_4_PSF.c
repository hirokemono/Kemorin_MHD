
/* draw_patches_4_PSF.c */

#include <OpenGL/gl3.h>
#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void release_PSF_texture_from_gl(struct psf_menu_val *psf_m){
	glDeleteTextures(1, &psf_m->texture_name[0]);
	return;
};

void draw_PSF_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, 
								   psf_s, psf_m, psf_a, psf_buf);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	if(num_patch <= 0) return;
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	Destroy_Phong_VAO(psf_VAO);
	
	return;	
}

void draw_PSF_texture_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int i;
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, psf_s, psf_m, psf_a, psf_buf);
	set_psf_textures_to_buf(ist_psf, ied_psf, psf_s, psf_a, psf_buf);
	
	glGenVertexArrays(1, &psf_VAO->id_VAO);
	glBindVertexArray(psf_VAO->id_VAO);
	
	glGenBuffers(1, &psf_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * psf_buf->num_nod_buf*psf_buf->ncomp_buf,
				 psf_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, psf_buf->istride,
						  (GLvoid*) (psf_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_norm * sizeof(GL_FLOAT)));
	glVertexAttribPointer(4, 2, GL_FLOAT, GL_FALSE, psf_buf->istride, 
						  (GLvoid*) (psf_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	glEnableVertexAttribArray(4);
	
	i = psf_a->ipsf_viz_far[ist_psf]-1;
	set_texture_to_buffer(psf_m[i]->texture_width, psf_m[i]->texture_height,
						  psf_m[i]->texture_rgba, psf_m[i]->texture_name);
	
	glBindVertexArray(0);
	
	if(num_patch <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	
	glUseProgram(kemo_shaders->phong_texure->programId);
	transfer_matrix_to_shader(kemo_shaders->phong_texure, view_s);
	set_phong_light_list(kemo_shaders->phong_texure, kemo_shaders->lights);
	
	int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
	glUniform1i(id_textureImage,0);
	
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	
	DestroyVBO(psf_VAO);
	
	return;	
}

void draw_PSF_arrow_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int ncorner = 20;
	int inum_buf;
	
	int num_patch = count_psf_arrows_to_buf(ncorner, psf_s, psf_m);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum_buf = set_psf_arrows_to_buf(ncorner, psf_s, psf_m, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	if(num_patch <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	Destroy_Phong_VAO(psf_VAO);
	
	return;	
}


void draw_PSF_isoline_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = 0;
	int inum = 0;
	int j, nd;
	double v_line;
	double f_color[4];
	
	num_patch = 2*count_PSF_all_isolines_to_buf(psf_s, psf_m);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum - set_PSF_all_isolines_to_buf(psf_s, psf_m, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	if(num_patch <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	Destroy_Phong_VAO(psf_VAO);
	
	return;
}
