
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
	glBindVertexArray(0);
	
	if(psf_VAO->npoint_draw <= 0) return;
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
	
	Const_VAO_4_Phong_Texture(psf_VAO, psf_buf);
	
	i = psf_a->ipsf_viz_far[ist_psf]-1;
	psf_m[i]->texture_name[0] = set_texture_to_buffer(psf_m[i]->texture_width, psf_m[i]->texture_height,
				psf_m[i]->texture_rgba);
	glBindVertexArray(0);
	
	if(psf_VAO->npoint_draw <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	
	glUseProgram(kemo_shaders->phong_texure->programId);
	transfer_matrix_to_shader(kemo_shaders->phong_texure, view_s);
	set_phong_light_list(kemo_shaders->phong_texure, kemo_shaders->lights);
	
	glBindTexture(GL_TEXTURE_2D, psf_m[i]->texture_name);
	int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
	glUniform1i(id_textureImage, 0);
	
	glBindBuffer(GL_ARRAY_BUFFER, psf_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	
//	Destroy_Phong_Texture_VAO(psf_VAO, psf_m[i]->texture_name);
	
	return;	
}

void draw_PSF_arrow_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int ncorner = 20;
	int i;
	int inum_buf;
	
	int num_patch = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
			num_patch = num_patch + count_psf_arrows_to_buf(ncorner, psf_s[i], psf_m[i]);
		};
	};
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum_buf = 0;
	for(i=0; i<psf_a->nmax_loaded; i++){
		if(psf_a->iflag_loaded[i]*psf_m[i]->draw_psf_vect != 0){
			inum_buf = set_psf_arrows_to_buf(inum_buf, ncorner, psf_s[i], psf_m[i], psf_buf);
		};
	};
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	glBindVertexArray(0);
	
	if(psf_VAO->npoint_draw <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	Destroy_Phong_VAO(psf_VAO);
	
	return;	
}


void draw_PSF_isoline_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int i, iflag;
	int inum_edge;
	
	int num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			num_patch = num_patch + count_PSF_all_isolines_to_buf(psf_s[i], psf_m[i]);
		};
	};
	num_patch = 2 * num_patch;
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum_edge = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			inum_edge = set_PSF_all_isolines_to_buf(inum_edge, psf_s[i], psf_m[i], psf_buf);
		};
	};
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	glBindVertexArray(0);
	
	if(psf_VAO->npoint_draw <= 0) return;
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, psf_VAO->npoint_draw);
	Destroy_Phong_VAO(psf_VAO);
	
	return;
}

int check_draw_psf(struct kemo_array_control *psf_a){
	int i;
    int iflag_psf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
	};
	return iflag_psf;
};

void draw_PSF_solid_objects_VAO(int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids *psf_solid_VAO, struct VAO_ids *psf_texture_VAO, 
			struct VAO_ids *psf_isoline_VAO, struct VAO_ids *psf_griph_VAO,
			struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *psf_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, psf_buf);
	alloc_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
		
	set_color_code_for_psfs(psf_s, psf_m, psf_a);
	
	draw_PSF_texture_VAO(shading_mode, IZERO, psf_a->istack_solid_psf_txtur, 
				psf_s, psf_m, psf_a, view_s, psf_texture_VAO, kemo_shaders, psf_buf);
	draw_PSF_patch_VAO(shading_mode, psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch, 
				psf_s, psf_m, psf_a, view_s, psf_solid_VAO, kemo_shaders, psf_buf);
	
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	draw_PSF_arrow_VAO(psf_s, psf_m, psf_a, view_s, 
						psf_griph_VAO, kemo_shaders, psf_buf);
	draw_PSF_isoline_VAO(psf_s, psf_m, psf_a, view_s, psf_isoline_VAO, kemo_shaders, psf_buf);
	
	free(psf_buf->v_buf);
	free(psf_buf);
	return;
}
