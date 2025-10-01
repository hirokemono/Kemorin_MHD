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
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_kemoview_tracer.h"
#include "m_phong_light_table_c.h"
#include "m_kemoview_PSF_buffers.h"
#include "m_kemoview_PSF_line_buffers.h"
#include "m_kemoview_map_buffers.h"
#include "m_kemoview_fieldline_buffers.h"
#include "m_kemoview_tracer_buffers.h"
#include "m_kemoview_mesh_buffers.h"
#include "m_kemoview_message_buffers.h"
#include "m_vertex_buffer.h"

#include "draw_map_4_PSF.h"
#include "draw_coastline.h"
#include "sort_by_patch_distance.h"
#include "draw_colorbar_gl.h"

struct kemoview_buffers{
    int nthreads;
    
    struct phong_lights *kemo_lights;
    
    struct gl_strided_buffer *PSF_node_buf;
    struct PSF_solid_buffers *PSF_solids;
    struct PSF_trans_buffers *PSF_transes;
    struct PSF_line_buffers  *PSF_lines;
    struct MAP_buffers       *MAP_bufs;
    
    struct FieldLine_buffers *Fline_bufs;

    struct Tracer_buffers *Tracer_bufs;
    
    struct MESH_buffers      *MESH_bufs;
    struct gl_strided_buffer *mesh_trns_buf;
    
    struct gl_strided_buffer *axis_buf;
    struct gl_index_buffer   *axis_index_buf;
    struct MESSAGE_buffers *MESSAGE_bufs;
    
    struct initial_cube_buffers *initial_bufs;
    
    struct gl_strided_buffer *screen_buf;
};


/* prototypes */

struct kemoview_buffers * init_kemoview_buffers(void);
void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers);

void set_number_of_threads(int input, struct kemoview_buffers *kemo_buffers);
int send_number_of_threads(struct kemoview_buffers *kemo_buffers);

void set_kemoviewer_buffers(struct kemoview_mul_psf *kemo_mul_psf,
                            struct kemoview_fline *kemo_fline,
                            struct kemoview_tracer *kemo_tracer,
                            struct kemoview_mesh *kemo_mesh,
                            struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers);
void set_transparent_buffers(struct kemoview_mul_psf *kemo_mul_psf,
                             struct kemoview_mesh *kemo_mesh,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers);
void set_fast_buffers(struct kemoview_mul_psf *kemo_mul_psf,
                      struct kemoview_fline *kemo_fline,
                      struct kemoview_mesh *kemo_mesh,
                      struct view_element *view_s,
                      struct kemoview_buffers *kemo_buffers);

#endif /* m_kemoview_object_buffers_h_ */
