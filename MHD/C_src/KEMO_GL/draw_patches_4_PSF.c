
/* draw_patches_4_PSF.c */

#include "draw_patches_4_PSF.h"

#define ARCPI 0.318309886

void release_PSF_texture_from_gl(struct psf_menu_val *psf_m){
	glDeleteTextures(1, &psf_m->texture_name[0]);
	return;
};

void const_PSF_patch_buffer(int shading_mode, int ist_psf, int ied_psf,
                            struct psf_data **psf_s, struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf){
	int num_vetex = ITHREE * count_psf_nodes_to_buf(ist_psf, ied_psf);
    set_buffer_address_4_patch(num_vetex, psf_buf);
	if(psf_buf->num_nod_buf <= 0) return;
	
	resize_strided_buffer(psf_buf);
	
	set_psf_nodes_to_buf(ist_psf, ied_psf, shading_mode, 
                         psf_s, psf_m, psf_a, psf_buf);
	return;
}

void const_PSF_texture_buffer(int shading_mode, int ist_psf, int ied_psf,
                              struct psf_data **psf_s, struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf){
    const_PSF_patch_buffer(shading_mode, ist_psf, ied_psf, psf_s, psf_m, psf_a, psf_buf);
    if(psf_buf->num_nod_buf > 0) set_psf_textures_to_buf(ist_psf, ied_psf, psf_s, psf_a, psf_buf);
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
	resize_strided_buffer(psf_buf);
	
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
	int i, iflag;
	int inum_patch;
	
	int num_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			num_patch = num_patch + count_PSF_all_isolines_to_buf(psf_s[i], psf_m[i]);
		};
	};
	psf_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf);
	
	inum_patch = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
		iflag = psf_a->iflag_loaded[i] * (psf_m[i]->draw_psf_grid+psf_m[i]->draw_psf_zero);
        if(iflag != 0){
			if(psf_m[i]->isoline_width <= 0.0){
				psf_m[i]->isoline_width = set_tube_radius_by_view(view_s, ref_width);
			};
			inum_patch = set_PSF_all_isolines_to_buf(inum_patch, psf_s[i], psf_m[i], psf_buf);
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

void const_PSF_solid_objects_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *solid_PSF_buf,
                                    struct gl_strided_buffer *stxur_PSF_buf){
    set_color_code_for_psfs(psf_s, psf_m, psf_a);
    
    const_PSF_texture_buffer(view_s->shading_mode, IZERO, psf_a->istack_solid_psf_txtur,
                             psf_s, psf_m, psf_a, stxur_PSF_buf);
        int i = psf_a->ipsf_viz_far[IZERO]-1;
        psf_m[i]->texture_name[0] = set_texture_to_buffer(psf_m[i]->texture_width,
                                                          psf_m[i]->texture_height,
                                                          psf_m[i]->texture_rgba);


    const_PSF_patch_buffer(view_s->shading_mode, psf_a->istack_solid_psf_txtur,
                           psf_a->istack_solid_psf_patch, psf_s, psf_m, psf_a,
                           solid_PSF_buf);
    return;
}

void set_PSF_solid_objects_VAO(struct view_element *view_s, struct psf_data **psf_s,
                               struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                               struct gl_strided_buffer *solid_PSF_buf, struct gl_strided_buffer *stxur_PSF_buf,
                               struct VAO_ids **psf_solid_VAO){
    psf_solid_VAO[1]->npoint_draw = stxur_PSF_buf->num_nod_buf;
    if(psf_solid_VAO[1]->npoint_draw > 0){ Const_VAO_4_Phong_Texture(psf_solid_VAO[1], stxur_PSF_buf);};
    psf_solid_VAO[0]->npoint_draw = solid_PSF_buf->num_nod_buf;
    if(psf_solid_VAO[0]->npoint_draw) {Const_VAO_4_Phong(psf_solid_VAO[0], solid_PSF_buf);};

    struct gl_strided_buffer *psf_buf
            = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    set_buffer_address_4_patch(3*128, psf_buf);
    alloc_strided_buffer(psf_buf);
    
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
	alloc_strided_buffer(psf_buf);
	
	
    const_PSF_texture_buffer(view_s->shading_mode,
                             psf_a->istack_solid_psf_patch, psf_a->istack_trans_psf_txtur,
                             psf_s, psf_m, psf_a, psf_buf);
        int i = psf_a->ipsf_viz_far[i]-1;
        psf_m[i]->texture_name[0] = set_texture_to_buffer(psf_m[i]->texture_width,
                                                          psf_m[i]->texture_height,
                                                          psf_m[i]->texture_rgba);

    const_PSF_patch_buffer(view_s->shading_mode, psf_a->istack_trans_psf_txtur,
                           psf_a->ntot_psf_patch, psf_s, psf_m, psf_a,
                           psf_buf);
    
    psf_trans_VAO[1]->npoint_draw = psf_buf->num_nod_buf;
    if(psf_trans_VAO[1]->npoint_draw > 0){ Const_VAO_4_Phong_Texture(psf_trans_VAO[1], psf_buf);};
    psf_trans_VAO[0]->npoint_draw = psf_buf->num_nod_buf;
    if(psf_trans_VAO[0]->npoint_draw) {Const_VAO_4_Phong(psf_trans_VAO[0], psf_buf);};

	free(psf_buf->v_buf);
	free(psf_buf);
	return;
};


void draw_PSF_solid_objects_VAO(struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct transfer_matrices *matrices, 
			struct VAO_ids **psf_solid_VAO, struct kemoview_shaders *kemo_shaders){
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	int i = psf_a->ipsf_viz_far[IZERO]-1;
	drawgl_textured_patches_VAO(&psf_m[i]->texture_name[0], matrices,
							  psf_solid_VAO[1], kemo_shaders);
	drawgl_patch_with_phong(matrices, psf_solid_VAO[0], kemo_shaders);
	return;
};

void draw_PSF_isolines_VAO(struct transfer_matrices *matrices,  struct VAO_ids **psf_solid_VAO,
						   struct kemoview_shaders *kemo_shaders){
    drawgl_patch_with_phong(matrices, psf_solid_VAO[2], kemo_shaders);
	drawgl_patch_with_phong(matrices, psf_solid_VAO[3], kemo_shaders);
	return;
}

void draw_PSF_trans_objects_VAO(struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
                                struct transfer_matrices *matrices, struct VAO_ids **psf_trans_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glDepthMask(GL_FALSE);
	glEnable(GL_BLEND);
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	int i = psf_a->ipsf_viz_far[IZERO]-1;
	drawgl_textured_patches_VAO(&psf_m[i]->texture_name[0], matrices,
							  psf_trans_VAO[1], kemo_shaders);
	drawgl_patch_with_phong(matrices, psf_trans_VAO[0], kemo_shaders);

	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	return;
};
