/*
//  m_vertex_buffer.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef m_vertex_buffer_h_
#define m_vertex_buffer_h_

#include <stdio.h>
#include <stdlib.h>

#define NPATCH_GL_BUFFER  4096

struct gl_strided_buffer{
    long nsize_buf;
    int istride;
    
    int ncomp_buf;
    int num_nod_buf;
    
    int ist_xyz;
    int ist_rtp;
    int ist_norm;
    int ist_tex;
    int ist_csurf;
    int ist_data;
    
    float *v_buf;
    
    float *x_draw;
    float *x_txur;
    float *n_draw;
    float *c_draw;
    float *d_draw;
};

/* prototypes */
long prod_padding_4096chars(long num_bytes);

struct gl_strided_buffer * init_strided_buffer(int num_points);
void alloc_strided_buffer(struct gl_strided_buffer *strided_buf);
void resize_strided_buffer(struct gl_strided_buffer *strided_buf);
void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf);

void set_buffer_address_4_patch(int num_points, struct gl_strided_buffer *strided_buf);
void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf);

void set_zero_stride_buffer(int inum, struct gl_strided_buffer *strided_buf);
void set_node_stride_buffer(int inum, struct gl_strided_buffer *strided_buf);
void select_strided_buffer(int inum, struct gl_strided_buffer *strided_buf);

#endif /* m_vertex_buffer_h_ */
