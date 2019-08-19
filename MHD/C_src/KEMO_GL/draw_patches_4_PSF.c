
/* draw_patches_4_PSF.c */

#include <OpenGL/gl3.h>
#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

static void set_texture(struct psf_menu_val *psf_m){
    static const GLfloat blend[] = {1.0,1.0,1.0,1.0};
	/* Preference for resiging texture */
	glBindTexture(GL_TEXTURE_2D , psf_m->texture_name[0]);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(
		GL_TEXTURE_2D , 0 , GL_RGBA , psf_m->texture_width , psf_m->texture_height ,
		0 , GL_RGBA , GL_UNSIGNED_BYTE , psf_m->texture_rgba);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, blend);
	
	return;
};

void draw_PSF_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, 
								   psf_s, psf_m, psf_a, psf_buf);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
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
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, psf_s, psf_m, psf_a, psf_buf);
	set_psf_textures_to_buf(ist_psf, ied_psf, psf_s, psf_a, psf_buf);
	
	glUseProgram(kemo_shaders->phong_texure->programId);
	int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
	
	transfer_matrix_to_shader(kemo_shaders->phong_texure, view_s);
	set_phong_light_list(kemo_shaders->phong_texure, kemo_shaders->lights);
	
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
	
	glActiveTexture(GL_TEXTURE_2D); 
	i = psf_a->ipsf_viz_far[ist_psf]-1;
	set_texture(psf_m[i]);
	glUniform1i(id_textureImage, 0);
	
	glBindVertexArray(0);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
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
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum_buf = set_psf_arrows_to_buf(ncorner, psf_s, psf_m, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
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
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum - set_PSF_all_isolines_to_buf(psf_s, psf_m, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(psf_VAO);
	
	return;
}
