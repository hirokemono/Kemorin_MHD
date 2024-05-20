/*
//  draw_PSF_patches_by_VAO.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef draw_PSF_patches_by_VAO_h_
#define draw_PSF_patches_by_VAO_h_

#include <stdio.h>

#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_transfer_matrices.h"
#include "m_phong_light_table_c.h"

#include "glsl.h"
#include "vartex_array_object_gl.h"
#include "drawGL_by_VAO.h"

/* prototypes */
void Const_VAO_Index_Simple(struct VAO_ids *VAO,
                            struct gl_strided_buffer *strided_buf,
                            struct gl_index_buffer *index_buf);
void Const_VAO_Index_Phong(struct VAO_ids *VAO,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_index_buffer *index_buf);
void drawgl_patch_index_phong(struct transfer_matrices *matrices,
                              struct phong_lights *lights,
                              struct kemoview_shaders *kemo_shaders,
                              struct VAO_ids *psf_solid_index_VAO);
void drawgl_textured_patches_index_VAO(GLuint *texture_name,
                                       struct transfer_matrices *matrices,
                                       struct phong_lights *lights,
                                       struct kemoview_shaders *kemo_shaders,
                                       struct VAO_ids *VAO);

void const_PSF_gl_texure_name(const int ipsf_texured,
                              struct gl_texure_image *psf_texure,
                              struct kemoview_shaders *kemo_shaders);

void release_PSF_texture_from_gl(const int ipsf_texured, struct kemoview_shaders *kemo_shaders);

void set_PSF_solid_objects_VAO(struct gl_strided_buffer *PSF_solid_buf, struct gl_strided_buffer *PSF_stxur_buf,
                               struct gl_strided_buffer *PSF_isotube_buf, struct gl_strided_buffer *PSF_isoline_buf,
                               struct gl_strided_buffer *PSF_arrow_buf, struct VAO_ids **psf_solid_VAO,
                               struct gl_strided_buffer *PSF_node_buf,
                               struct gl_index_buffer *PSF_solid_index_buf, struct gl_index_buffer *PSF_stxur_index_buf,
                               struct VAO_ids **psf_solid_index_VAO);

void set_PSF_trans_objects_VAO(struct gl_strided_buffer *PSF_trns_buf,
                               struct gl_strided_buffer *PSF_ttxur_buf,
                               struct gl_strided_buffer *PSF_node_buf,
                               struct gl_index_buffer   *PSF_trns_index_buf,
                               struct gl_index_buffer   *PSF_ttxur_index_buf,
                               struct VAO_ids **psf_trans_VAO,
                               struct VAO_ids **psf_trans_index_VAO);

void draw_PSF_solid_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_solid_VAO,
                                struct VAO_ids **psf_solid_index_VAO,
                                struct kemoview_shaders *kemo_shaders);
void draw_PSF_trans_objects_VAO(struct transfer_matrices *matrices,
                                struct phong_lights *lights,
                                struct VAO_ids **psf_trans_VAO,
                                struct VAO_ids **psf_trans_index_VAO,
                                struct kemoview_shaders *kemo_shaders);
#endif /* draw_PSF_patches_by_VAO_h_ */
