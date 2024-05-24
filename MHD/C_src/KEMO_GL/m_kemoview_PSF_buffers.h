/*
//  m_kemoview_PSF_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_PSF_BUFFERS_
#define M_KEMOVIEW_PSF_BUFFERS_

#include <stdio.h>
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_psf.h"
#include "m_vertex_buffer.h"

#include "draw_patches_4_PSF.h"

struct PSF_solid_buffers{
    struct gl_index_buffer *PSF_solid_index_buf;
    struct gl_index_buffer *PSF_stxur_index_buf;
    
    struct gl_strided_buffer *PSF_solid_buf;
    struct gl_strided_buffer *PSF_stxur_buf;
};

struct PSF_trans_buffers{
    struct gl_index_buffer *PSF_trns_index_buf;
    struct gl_index_buffer *PSF_ttxur_index_buf;
    
    struct gl_strided_buffer *PSF_trns_buf;
    struct gl_strided_buffer *PSF_ttxur_buf;
};


/* prototypes */

struct PSF_solid_buffers * init_PSF_solid_buffers(void);
struct PSF_trans_buffers * init_PSF_trans_buffers(void);

void dealloc_PSF_solid_buffers(struct PSF_solid_buffers *PSF_solids);
void dealloc_PSF_trans_buffers(struct PSF_trans_buffers *PSF_transes);

void const_PSF_solid_objects_buffer(const int nthreads,
                                    struct view_element *view_s,
                                    struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m,
                                    struct kemo_array_control *psf_a,
                                    struct PSF_solid_buffers *PSF_solids);
void const_PSF_trans_objects_buffer(const int nthreads, 
                                    struct view_element *view_s,
                                    struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, 
                                    struct kemo_array_control *psf_a,
                                    struct PSF_trans_buffers *PSF_transes);

#endif /* M_KEMOVIEW_PSF_BUFFERS_ */
