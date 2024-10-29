/*
//  draw_PSF_patches_by_VAO.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "draw_PSF_patches_by_VAO.h"


void const_PSF_gl_texure_name(const int ipsf_texured,
                              struct gl_texure_image *psf_texure,
                              struct kemoview_shaders *kemo_shaders){
    if(ipsf_texured > -1){
        kemo_shaders->texture_name = set_texture_to_buffer(psf_texure);
    };
    return;
};



void release_PSF_texture_from_gl(const int ipsf_texured, struct kemoview_shaders *kemo_shaders){
    if(ipsf_texured > 0){glDeleteTextures(1, &kemo_shaders->texture_name);};
    return;
};

void set_PSF_solid_objects_VAO(struct gl_strided_buffer *PSF_node_buf,
                               struct PSF_solid_buffers *PSF_solids,
                               struct VAO_ids **psf_solid_VAO,
                               struct VAO_ids **psf_solid_index_VAO){
    Const_VAO_4_Phong_Texture(psf_solid_VAO[1], PSF_solids->PSF_stxur_buf);
    Const_Phong_VAO(psf_solid_VAO[0], PSF_solids->PSF_solid_buf);

    Const_Phong_Index_VAO(psf_solid_index_VAO[0], PSF_node_buf,
                          PSF_solids->PSF_solid_index_buf);
    Const_VAO_Index_Phong_Texture(psf_solid_index_VAO[1], PSF_node_buf,
                                  PSF_solids->PSF_stxur_index_buf);

    return;
};

void set_PSF_trans_objects_VAO(struct gl_strided_buffer *PSF_node_buf,
                               struct PSF_trans_buffers *PSF_transes,
                               struct VAO_ids **psf_trans_VAO,
                               struct VAO_ids **psf_trans_index_VAO){
    Const_Phong_Index_VAO(psf_trans_index_VAO[0], PSF_node_buf,
                          PSF_transes->PSF_trns_index_buf);
    Const_VAO_Index_Phong_Texture(psf_trans_index_VAO[1], PSF_node_buf,
                                  PSF_transes->PSF_ttxur_index_buf);

    Const_Phong_VAO(psf_trans_VAO[0], PSF_transes->PSF_trns_buf);
    Const_VAO_4_Phong_Texture(psf_trans_VAO[1], PSF_transes->PSF_ttxur_buf);
    return;
};

void set_PSF_line_objects_VAO(struct PSF_line_buffers *PSF_lines,
                              struct VAO_ids **psf_solid_index_VAO,
                              struct VAO_ids *psf_lines_VAO,
                              struct VAO_ids *grid_line_VAO,
                              struct VAO_ids *grid_tube_VAO){
    Const_Simple_VAO(psf_lines_VAO, PSF_lines->PSF_isoline_buf);
    Const_VAO_Index_Phong_Texture(psf_solid_index_VAO[4],
                                  PSF_lines->PSF_isotube_buf,
                                  PSF_lines->PSF_isotube_index_buf);
    Const_VAO_Index_Phong_Texture(psf_solid_index_VAO[5],
                                  PSF_lines->PSF_arrow_buf,
                                  PSF_lines->PSF_arrow_index_buf);
    
    Const_Simple_VAO(grid_line_VAO, PSF_lines->coast_line_buf);
    Const_VAO_Index_Phong_Texture(grid_tube_VAO,
                                  PSF_lines->coast_tube_buf,
                                  PSF_lines->coast_index_buf);
    return;
};

void draw_PSF_solid_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_solid_VAO,
                                struct VAO_ids **psf_solid_index_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glDisable(GL_CULL_FACE);
    drawgl_textured_patches_VAO(&kemo_shaders->texture_name, matrices,
                                lights, kemo_shaders, psf_solid_VAO[1]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[0]);

    drawgl_elements_with_phong(matrices, lights, kemo_shaders, psf_solid_index_VAO[0]);
    drawgl_textured_elements_VAO(&kemo_shaders->texture_name, matrices, lights,
                                 kemo_shaders, psf_solid_index_VAO[1]);
    glDisable(GL_CULL_FACE);
    drawgl_elements_with_phong(matrices, lights, kemo_shaders, psf_solid_index_VAO[4]);
    drawgl_elements_with_phong(matrices, lights, kemo_shaders, psf_solid_index_VAO[5]);
    return;
};

void draw_PSF_trans_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_trans_VAO,
                                struct VAO_ids **psf_trans_index_VAO,
                                struct kemoview_shaders *kemo_shaders){
    glDepthMask(GL_FALSE);
    glEnable(GL_BLEND);
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    drawgl_textured_elements_VAO(&kemo_shaders->texture_name, matrices, lights,
                                 kemo_shaders, psf_trans_index_VAO[1]);
    drawgl_elements_with_phong(matrices, lights, kemo_shaders, psf_trans_index_VAO[0]);
    
    drawgl_textured_patches_VAO(&kemo_shaders->texture_name, matrices,
                                lights, kemo_shaders, psf_trans_VAO[1]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_trans_VAO[0]);

    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
    return;
};
