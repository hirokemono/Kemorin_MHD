/*
//  draw_PSF_patches_by_VAO.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "draw_PSF_patches_by_VAO.h"


void const_PSF_gl_texure_name(struct kemo_PSF_texure *psf_texure,
                              struct kemoview_shaders *kemo_shaders){
    if(psf_texure->ipsf_texured > -1){
        kemo_shaders->texture_name = set_texture_to_buffer(psf_texure->texure_width,
                                                           psf_texure->texure_height,
                                                           psf_texure->texure_rgba);
    };
    return;
};

void release_PSF_texture_from_gl(struct kemo_PSF_texure *psf_texure,
                                 GLuint *texture_name){
    if(psf_texure->ipsf_texured > 0){glDeleteTextures(1, texture_name);};
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


void draw_PSF_solid_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_solid_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glDisable(GL_CULL_FACE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    
    drawgl_textured_patches_VAO(&kemo_shaders->texture_name, matrices,
                                lights, kemo_shaders, psf_solid_VAO[1]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[0]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[2]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[3]);
    return;
};

void draw_PSF_trans_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_trans_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glDepthMask(GL_FALSE);
    glEnable(GL_BLEND);
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    
    drawgl_textured_patches_VAO(&kemo_shaders->texture_name, matrices,
                                lights, kemo_shaders, psf_trans_VAO[1]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_trans_VAO[0]);

    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
    return;
};
