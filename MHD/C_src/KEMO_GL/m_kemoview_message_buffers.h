/*
//  m_kemoview_message_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_MESSAGE_BUFFERS_
#define M_KEMOVIEW_MESSAGE_BUFFERS_

#include <stdio.h>
#include "m_kemoview_psf_menu.h"
#include "m_colorbar_buffer.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_mesh.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_kemoview_tracer.h"
#include "m_vertex_buffer.h"
#include "draw_colorbar_gl.h"

struct MESSAGE_buffers{
    struct gl_strided_buffer *cbar_buf;

    struct gl_textbox_buffer *cbar_min_buf;
    struct gl_textbox_buffer *cbar_max_buf;
    struct gl_textbox_buffer *cbar_zero_buf;

    struct gl_textbox_buffer *timelabel_buf;
    struct gl_textbox_buffer *message_buf;
};

/*  prototypes */

struct MESSAGE_buffers * init_MESSAGE_buffers(void);
void dealloc_MESSAGE_buffers(struct MESSAGE_buffers *MESSAGE_bufs);


void select_colorbar_box_buffer(int iflag_retina, int nx_win, int ny_win,
                                float text_color[4], float bg_color[4],
                                struct kemoview_mul_psf *kemo_mul_psf,
                                struct kemoview_fline *kemo_fline,
                                struct kemoview_tracer *kemo_tracer,
                                struct MESSAGE_buffers *MESSAGE_bufs,
                                struct cbar_work *cbar_wk);

void const_message_buffers(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct kemoview_mul_psf *kemo_mul_psf,
                           struct kemoview_fline *kemo_fline,
                           struct kemoview_tracer *kemo_tracer,
                           struct kemoview_mesh *kemo_mesh,
                           struct view_element *view_s,
                           struct MESSAGE_buffers *MESSAGE_bufs);

#endif /*  M_KEMOVIEW_MESSAGE_BUFFERS_  */
