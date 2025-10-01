/*
//  m_kemoview_fieldline_buffers.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef M_KEMOVIEW_FIELDLINE_BUFFERS_
#define M_KEMOVIEW_FIELDLINE_BUFFERS_

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


struct FieldLine_buffers{
    struct gl_strided_buffer *FLINE_line_buf;
    
    struct gl_strided_buffer *FLINE_tube_buf;
    struct gl_index_buffer *FLINE_tube_index_buf;
};

/* prototypes */
struct FieldLine_buffers * init_FieldLine_buffers(void);
void dealloc_FieldLine_buffers(struct FieldLine_buffers *Fline_bufs);

void const_fieldlines_buffer(const int nthreads, struct view_element *view_s,
                             struct psf_data *fline_d,
                             struct fline_directions *fline_dir,
                             struct psf_menu_val *fline_m,
                             struct FieldLine_buffers *Fline_bufs);

#endif   /* M_KEMOVIEW_FIELDLINE_BUFFERS_ */
