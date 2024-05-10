/*
//  m_kemoview_object_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef m_kemoview_object_buffers_h_
#define m_kemoview_object_buffers_h_

#include <stdio.h>
#include "set_cube_to_buf.h"
#include "m_kemoview_psf_menu.h"
#include "m_colorbar_buffer.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_mesh.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_phong_light_table_c.h"
#include "m_vertex_buffer.h"

#include "draw_map_4_PSF.h"
#include "draw_fieldlines.h"
#include "draw_patch_4_mesh_c.h"
#include "draw_colorbar_gl.h"
#include "draw_coastline.h"
#include "sort_by_patch_distance.h"

struct kemoview_buffers{
    int nthreads;
    
    struct phong_lights *kemo_lights;
    
    struct gl_strided_buffer *cube_buf;
    struct gl_index_buffer *cube_index_buf;
    
    struct gl_strided_buffer *PSF_solid_buf;
    struct gl_strided_buffer *PSF_trns_buf;
    struct gl_strided_buffer *PSF_stxur_buf;
    struct gl_strided_buffer *PSF_ttxur_buf;
    struct gl_strided_buffer *PSF_arrow_buf;
    struct gl_strided_buffer *PSF_isoline_buf;
    
    struct gl_strided_buffer *MAP_solid_buf;
    struct gl_strided_buffer *MAP_isoline_buf;
    
    struct gl_strided_buffer *FLINE_line_buf;
    struct gl_strided_buffer *FLINE_tube_buf;
    
    struct gl_strided_buffer *mesh_solid_buf;
    struct gl_strided_buffer *mesh_grid_buf;
    struct gl_strided_buffer *mesh_node_buf;
    struct gl_strided_buffer *mesh_trns_buf;
    
    struct gl_strided_buffer *coast_buf;
    struct gl_strided_buffer *sph_grid_buf;
    
    int ncorner_axis;
    struct gl_strided_buffer *axis_buf;
    

    struct gl_strided_buffer *screen_buf;
    
    struct gl_strided_buffer *cbar_buf;
    
    struct gl_textbox_buffer *cbar_min_buf;
    struct gl_textbox_buffer *cbar_max_buf;
    struct gl_textbox_buffer *cbar_zero_buf;
    struct gl_textbox_buffer *timelabel_buf;
    struct gl_textbox_buffer *message_buf;
};


/* prototypes */

struct kemoview_buffers * init_kemoview_buffers(void);
void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers);

void set_number_of_threads(int input, struct kemoview_buffers *kemo_buffers);
int send_number_of_threads(struct kemoview_buffers *kemo_buffers);

void set_kemoviewer_buffers(struct kemoview_psf *kemo_psf, struct kemoview_fline *kemo_fline,
                            struct kemoview_mesh *kemo_mesh, struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers);
void set_transparent_buffers(struct kemoview_psf *kemo_psf,
                             struct kemoview_mesh *kemo_mesh,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers);
void set_fast_buffers(struct kemoview_psf *kemo_psf, struct kemoview_mesh *kemo_mesh,
                      struct view_element *view_s, struct kemoview_buffers *kemo_buffers);

#endif /* m_kemoview_object_buffers_h_ */
