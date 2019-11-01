
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void release_PSF_texture_from_gl(struct psf_menu_val *psf_m){
	glDeleteTextures(1, &psf_m->texture_name[0]);
	return;
};

void set_PSF_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *psf_buf){
	int num_patch = count_psf_nodes_to_buf(ist_psf, ied_psf);
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, 
								   psf_s, psf_m, psf_a, psf_buf);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	return;	
}

void set_PSF_texture_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, 
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *psf_buf){
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
	
	return;	
}

void set_PSF_arrow_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *psf_buf){
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
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	return;	
}


void set_PSF_isoline_VAO(struct view_element *view_s, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct VAO_ids *psf_VAO, struct gl_strided_buffer *psf_buf){
	double ref_width = 1.5;
	int ncorner = 6;
	int i, iflag;
	int inum_patch;
	
	int num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			num_patch = num_patch + count_PSF_all_isolines_to_buf(ncorner, psf_s[i], psf_m[i]);
		};
	};
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->isoline_width <= 0.0){
				psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
			};
			inum_patch = set_PSF_all_isolines_to_buf(inum_patch, ncorner,
													 psf_s[i], psf_m[i], psf_buf);
		};
	};
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
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

void set_PSF_solid_objects_VAO(struct view_element *view_s, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct VAO_ids **psf_solid_VAO){
	struct gl_strided_buffer *psf_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, psf_buf);
	alloc_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
		
	set_color_code_for_psfs(psf_s, psf_m, psf_a);
	
	set_PSF_texture_VAO(view_s->shading_mode,
				IZERO, psf_a->istack_solid_psf_txtur, 
				psf_s, psf_m, psf_a, psf_solid_VAO[1], psf_buf);
	
	set_PSF_patch_VAO(view_s->shading_mode,
				psf_a->istack_solid_psf_txtur, psf_a->istack_solid_psf_patch, 
				psf_s, psf_m, psf_a, psf_solid_VAO[0], psf_buf);
	
	set_PSF_arrow_VAO(psf_s, psf_m, psf_a, psf_solid_VAO[3], psf_buf);
	
	set_PSF_isoline_VAO(view_s, psf_s, psf_m, psf_a, psf_solid_VAO[2], psf_buf);
	
	free(psf_buf->v_buf);
	free(psf_buf);
	return;
};

void set_PSF_trans_objects_VAO(struct view_element *view_s, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct VAO_ids **psf_trans_VAO){
	struct gl_strided_buffer *psf_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(3*128, psf_buf);
	alloc_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	
	set_PSF_texture_VAO(view_s->shading_mode, 
				psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur, 
				psf_s, psf_m, psf_a, psf_trans_VAO[1], psf_buf);
	set_PSF_patch_VAO(view_s->shading_mode, 
				psf_a->istack_trans_psf_txtur, psf_a->ntot_psf_patch,
				psf_s, psf_m, psf_a, psf_trans_VAO[0], psf_buf);
	
	free(psf_buf->v_buf);
	free(psf_buf);
	return;
};


void draw_PSF_solid_objects_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids **psf_solid_VAO, struct kemoview_shaders *kemo_shaders){
	
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	if(psf_solid_VAO[1]->npoint_draw >0){
		glUseProgram(kemo_shaders->phong_texure->programId);
		transfer_matrix_to_shader(kemo_shaders->phong_texure, view_s);
		set_phong_light_list(kemo_shaders->phong_texure, kemo_shaders->lights);
		
		glBindVertexArray(psf_solid_VAO[1]->id_VAO);
		
		int i = psf_a->ipsf_viz_far[IZERO]-1;
		glBindTexture(GL_TEXTURE_2D, psf_m[i]->texture_name[0]);
		int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
		glUniform1i(id_textureImage, 0);
		
		glBindBuffer(GL_ARRAY_BUFFER, psf_solid_VAO[1]->id_vertex);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_solid_VAO[1]->npoint_draw);
	};
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	if(psf_solid_VAO[0]->npoint_draw > 0){
		glBindVertexArray(psf_solid_VAO[0]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_solid_VAO[0]->npoint_draw);
	}
	return;
};

void draw_PSF_isolines_VAO(struct view_element *view_s, 
			struct VAO_ids **psf_solid_VAO, struct kemoview_shaders *kemo_shaders){
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	if(psf_solid_VAO[2]->npoint_draw > 0){
		glBindVertexArray(psf_solid_VAO[2]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_solid_VAO[2]->npoint_draw);
	};
	if(psf_solid_VAO[3]->npoint_draw > 0){
		glBindVertexArray(psf_solid_VAO[3]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_solid_VAO[3]->npoint_draw);
	};
	return;
}

void draw_PSF_trans_objects_VAO(struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids **psf_trans_VAO, struct kemoview_shaders *kemo_shaders){
	glDepthMask(GL_FALSE);
	glEnable(GL_BLEND);
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	if(psf_trans_VAO[1]->npoint_draw >0){
		glBindVertexArray(psf_trans_VAO[1]->id_VAO);
		
		glUseProgram(kemo_shaders->phong_texure->programId);
		transfer_matrix_to_shader(kemo_shaders->phong_texure, view_s);
		set_phong_light_list(kemo_shaders->phong_texure, kemo_shaders->lights);
		
		int i = psf_a->ipsf_viz_far[IZERO]-1;
		glBindTexture(GL_TEXTURE_2D, psf_m[i]->texture_name[0]);
		int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
		glUniform1i(id_textureImage, 0);
		
		glBindBuffer(GL_ARRAY_BUFFER, psf_trans_VAO[1]->id_vertex);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_trans_VAO[1]->npoint_draw);
	};
	
	if(psf_trans_VAO[0]->npoint_draw > 0){
		glUseProgram(kemo_shaders->phong->programId);
		transfer_matrix_to_shader(kemo_shaders->phong, view_s);
		set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
		glBindVertexArray(psf_trans_VAO[0]->id_VAO);
		glDrawArrays(GL_TRIANGLES, IZERO, psf_trans_VAO[0]->npoint_draw);
	}
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	return;
};
