/*
//  m_kemoview_tracer_buffers.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef M_KEMOVIEW_TRACER_BUFFERS_
#define M_KEMOVIEW_TRACER_BUFFERS_

#include "m_kemoview_fline_menu.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "set_fieldline_to_buf.h"
#include "pthread_fieldline_to_buf.h"
#include "set_axis_to_buf.h"

#include "icosahedron_c.h"

struct Tracer_buffers{
    struct gl_strided_buffer *Tracer_dot_buf;

    struct gl_strided_buffer *Tracer_ico_buf;
    struct gl_index_buffer   *Tracer_index_buf;
};

/* prototypes */
struct Tracer_buffers * init_Tracer_buffers(void);
void dealloc_Tracer_buffers(struct Tracer_buffers *Tracer_bufs);

void const_tracer_buffer(const int nthreads, struct view_element *view_s,
                         struct psf_data *tracer_d,
                         struct psf_menu_val *tracer_m,
                         struct Tracer_buffers *Tracer_bufs);

#endif /*  M_KEMOVIEW_TRACER_BUFFERS_  */
