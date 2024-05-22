/*
//  m_kemoview_PSF_line_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_PSF_LINE_BUFFERS_
#define M_KEMOVIEW_PSF_LINE_BUFFERS_

#include <stdio.h>
#include "set_cube_to_buf.h"
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_mesh_menu.h"
#include "m_vertex_buffer.h"

#include "draw_coastline.h"

struct PSF_line_buffers{
    long num_isoline_buf;
    struct gl_strided_buffer *PSF_isoline_buf;
    struct gl_strided_buffer *PSF_isotube_buf;
    struct gl_strided_buffer *PSF_arrow_buf;
    
    struct gl_strided_buffer *coast_line_buf;
    struct gl_strided_buffer *coast_tube_buf;
};

/* prototypes */

struct PSF_line_buffers * init_PSF_line_buffers(void);
void dealloc_PSF_line_buffers(struct PSF_line_buffers *PSF_lines);

void const_PSF_isolines_buffer(const int nthreads,
                               struct view_element *view_s, struct psf_data **psf_s,
                               struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                               struct mesh_menu_val *mesh_m, 
                               struct PSF_line_buffers *PSF_lines);
void set_isolines_for_fast_draw(struct mesh_menu_val *mesh_m, 
                                struct PSF_line_buffers *PSF_lines);


#endif /* M_KEMOVIEW_PSF_LINE_BUFFERS_ */
