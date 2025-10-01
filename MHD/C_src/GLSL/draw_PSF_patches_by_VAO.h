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
#include "m_kemoview_object_buffers.h"

#include "glsl.h"
#include "vartex_array_object_gl.h"
#include "drawGL_by_VAO.h"

/* prototypes */
void const_PSF_gl_texure_name(const int ipsf_texured,
                              struct gl_texure_image *psf_texure,
                              struct kemoview_shaders *kemo_shaders);

void release_PSF_texture_from_gl(const int ipsf_texured, struct kemoview_shaders *kemo_shaders);

void set_PSF_solid_objects_VAO(struct gl_strided_buffer *PSF_node_buf,
                               struct PSF_solid_buffers *PSF_solids,
                               struct VAO_ids **psf_solid_VAO,
                               struct VAO_ids **psf_solid_index_VAO);

void set_PSF_trans_objects_VAO(struct gl_strided_buffer *PSF_node_buf,
                               struct PSF_trans_buffers *PSF_transes,
                               struct VAO_ids **psf_trans_VAO,
                               struct VAO_ids **psf_trans_index_VAO);

void set_PSF_line_objects_VAO(struct PSF_line_buffers *PSF_lines,
                              struct VAO_ids **psf_solid_index_VAO,
                              struct VAO_ids *psf_lines_VAO,
                              struct VAO_ids *grid_line_VAO,
                              struct VAO_ids *grid_tube_VAO);

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
