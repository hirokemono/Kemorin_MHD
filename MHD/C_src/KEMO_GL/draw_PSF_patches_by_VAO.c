/*
//  draw_PSF_patches_by_VAO.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "draw_PSF_patches_by_VAO.h"

void release_PSF_texture_from_gl(struct psf_menu_val *psf_m){
    glDeleteTextures(1, &psf_m->texture_name[0]);
    return;
};

void set_PSF_solid_objects_VAO(struct gl_strided_buffer *PSF_solid_buf, struct gl_strided_buffer *PSF_stxur_buf,
                               struct gl_strided_buffer *PSF_isoline_buf, struct gl_strided_buffer *PSF_arrow_buf,
                               struct VAO_ids **psf_solid_VAO){
    Const_VAO_4_Phong_Texture(psf_solid_VAO[1], PSF_stxur_buf);
    Const_VAO_4_Phong(psf_solid_VAO[0], PSF_solid_buf);
    Const_VAO_4_Phong(psf_solid_VAO[2], PSF_isoline_buf);
    Const_VAO_4_Phong(psf_solid_VAO[3], PSF_arrow_buf);
    return;
};

void set_PSF_trans_objects_VAO(struct gl_strided_buffer *PSF_trns_buf, struct gl_strided_buffer *PSF_ttxur_buf,
                               struct VAO_ids **psf_trans_VAO){
    Const_VAO_4_Phong(psf_trans_VAO[0], PSF_trns_buf);
    Const_VAO_4_Phong_Texture(psf_trans_VAO[1], PSF_ttxur_buf);
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
    drawgl_patch_with_phong(matrices, psf_solid_VAO[2], kemo_shaders);
    drawgl_patch_with_phong(matrices, psf_solid_VAO[3], kemo_shaders);
    return;
};

void draw_PSF_trans_objects_VAO(struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                struct transfer_matrices *matrices, struct VAO_ids **psf_trans_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glDepthMask(GL_FALSE);
    glEnable(GL_BLEND);
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
    return;
};
