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


void Const_VAO_Index_Phong(struct VAO_ids *VAO,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_index_buffer *index_buf){
    VAO->npoint_draw = index_buf->ntot_vertex;
    if(VAO->npoint_draw <= 0) return;

    GLenum ErrorCheckValue = glGetError();

    glDeleteBuffers(1, &VAO->id_vertex);

    glGenBuffers(1, &VAO->id_vertex);
    glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
                 strided_buf->v_buf, GL_STATIC_DRAW);
    
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
    
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glBindVertexArray(0);

    glBindVertexArray(VAO->id_VAO);
    glDeleteBuffers(1, &VAO->id_index);

/* Create index buffer on GPU, and then copy from CPU */
    glGenBuffers(1, &VAO->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, (VAO->npoint_draw * sizeof(unsigned int)),
                 index_buf->ie_buf, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    return;
};



void Const_VAO_Index_Phong_Texture(struct VAO_ids *VAO,
                                   struct gl_strided_buffer *strided_buf,
                                   struct gl_index_buffer *index_buf){
    VAO->npoint_draw = index_buf->ntot_vertex;
    if(VAO->npoint_draw <= 0) return;

    glBindVertexArray(VAO->id_VAO);
    glDeleteBuffers(1, &VAO->id_vertex);
    
    glGenBuffers(1, &VAO->id_vertex);
    glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
                 strided_buf->v_buf, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, strided_buf->istride,
                          (GLvoid*) (strided_buf->ist_tex * sizeof(GL_FLOAT)));

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);

/* Create index buffer on GPU, and then copy from CPU */
    glGenBuffers(1, &VAO->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, (VAO->npoint_draw * sizeof(unsigned int)),
                 index_buf->ie_buf, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    return;
};


void release_PSF_texture_from_gl(const int ipsf_texured, struct kemoview_shaders *kemo_shaders){
    if(ipsf_texured > 0){glDeleteTextures(1, &kemo_shaders->texture_name);};
    return;
};

void set_PSF_solid_objects_VAO(struct gl_strided_buffer *PSF_solid_buf, struct gl_strided_buffer *PSF_stxur_buf,
                               struct gl_strided_buffer *PSF_isotube_buf, struct gl_strided_buffer *PSF_isoline_buf,
                               struct gl_strided_buffer *PSF_arrow_buf, struct VAO_ids **psf_solid_VAO,
                               struct gl_strided_buffer *PSF_node_buf,
                               struct gl_index_buffer *PSF_solid_index_buf, struct gl_index_buffer *PSF_stxur_index_buf,
                               struct VAO_ids **psf_solid_index_VAO){
    Const_VAO_4_Phong_Texture(psf_solid_VAO[1], PSF_stxur_buf);
    Const_VAO_4_Phong(psf_solid_VAO[0], PSF_solid_buf);
    Const_VAO_4_Phong(psf_solid_VAO[2], PSF_isotube_buf);
    Const_VAO_4_Phong(psf_solid_VAO[3], PSF_arrow_buf);

    Const_VAO_Index_Phong(psf_solid_index_VAO[0], PSF_node_buf, PSF_solid_index_buf);
    Const_VAO_Index_Phong_Texture(psf_solid_index_VAO[1], PSF_node_buf, PSF_stxur_index_buf);

    return;
};

void set_PSF_trans_objects_VAO(struct gl_strided_buffer *PSF_trns_buf,
                               struct gl_strided_buffer *PSF_ttxur_buf,
                               struct gl_strided_buffer *PSF_node_buf,
                               struct gl_index_buffer   *PSF_trns_index_buf,
                               struct gl_index_buffer   *PSF_ttxur_index_buf,
                               struct VAO_ids **psf_trans_VAO,
                               struct VAO_ids **psf_trans_index_VAO){
    Const_VAO_Index_Phong(psf_trans_index_VAO[0], PSF_node_buf, PSF_trns_index_buf);
    Const_VAO_Index_Phong_Texture(psf_trans_index_VAO[1], PSF_node_buf, PSF_ttxur_index_buf);

    Const_VAO_4_Phong(psf_trans_VAO[0], PSF_trns_buf);
    Const_VAO_4_Phong_Texture(psf_trans_VAO[1], PSF_ttxur_buf);
    return;
};


void drawgl_patch_index_phong(struct transfer_matrices *matrices,
                              struct phong_lights *lights,
                              struct kemoview_shaders *kemo_shaders,
                              struct VAO_ids *VAO){
    if(VAO->npoint_draw <= 0) return;


    glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
    set_phong_light_list(kemo_shaders->phong, lights);

    glBindVertexArray(VAO->id_VAO);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glDrawElements(GL_TRIANGLES, VAO->npoint_draw , GL_UNSIGNED_INT, 0);
    return;
}

void drawgl_textured_patches_index_VAO(GLuint *texture_name,
                                       struct transfer_matrices *matrices,
                                       struct phong_lights *lights,
                                       struct kemoview_shaders *kemo_shaders,
                                       struct VAO_ids *VAO){
    if(VAO->npoint_draw <= 0) return;
    
    glUseProgram(kemo_shaders->phong_texure->programId);
    transfer_matrix_to_GL(kemo_shaders->phong_texure, matrices);
    set_phong_light_list(kemo_shaders->phong_texure, lights);

    glBindVertexArray(VAO->id_VAO);
    
    glBindTexture(GL_TEXTURE_2D, *texture_name);
    int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
    glUniform1i(id_textureImage, 0);
    
    glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glDrawElements(GL_TRIANGLES, VAO->npoint_draw , GL_UNSIGNED_INT, 0);
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

    drawgl_patch_index_phong(matrices, lights, kemo_shaders, psf_solid_index_VAO[0]);
    drawgl_textured_patches_index_VAO(&kemo_shaders->texture_name, matrices, lights,
                                      kemo_shaders, psf_solid_index_VAO[1]);
    glDisable(GL_CULL_FACE);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[2]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_solid_VAO[3]);
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

    drawgl_textured_patches_index_VAO(&kemo_shaders->texture_name, matrices, lights,
                                      kemo_shaders, psf_trans_index_VAO[1]);
    drawgl_patch_index_phong(matrices, lights, kemo_shaders, psf_trans_index_VAO[0]);
    
    drawgl_textured_patches_VAO(&kemo_shaders->texture_name, matrices,
                                lights, kemo_shaders, psf_trans_VAO[1]);
    drawgl_patch_with_phong(matrices, lights, kemo_shaders, psf_trans_VAO[0]);

    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
    return;
};
