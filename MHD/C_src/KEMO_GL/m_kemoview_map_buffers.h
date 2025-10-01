/*
//  m_kemoview_map_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_MAP_BUFFERS_
#define M_KEMOVIEW_MAP_BUFFERS_

#include <stdio.h>
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_mesh_menu.h"
#include "m_vertex_buffer.h"

#include "draw_map_4_PSF.h"
#include "draw_coastline.h"
#include "draw_patches_4_PSF.h"

struct MAP_buffers{
    struct gl_index_buffer   *MAP_solid_index_buf;
    struct gl_strided_buffer *MAP_solid_buf;
    struct gl_strided_buffer *MAP_isoline_buf;
    struct gl_index_buffer *MAP_isotube_index_buf;
    
    struct gl_strided_buffer *MAP_coast_line_buf;
    struct gl_strided_buffer *MAP_coast_tube_buf;
    struct gl_index_buffer   *MAP_coast_index_buf;
};
 

/* prototypes */

struct MAP_buffers * init_MAP_buffers(void);
void dealloc_MAP_buffers(struct MAP_buffers *MAP_bufs);
void const_map_buffers(int nthreads, struct kemoview_mul_psf *kemo_mul_psf,
                       struct mesh_menu_val *mesh_m, struct view_element *view_s,
                       struct gl_strided_buffer *PSF_node_buf, struct MAP_buffers *MAP_bufs);

#endif /* M_KEMOVIEW_MAP_BUFFERS_ */
