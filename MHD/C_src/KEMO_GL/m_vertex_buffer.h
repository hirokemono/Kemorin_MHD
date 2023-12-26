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
    long num_nod_buf;
    
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

struct gl_index_buffer{
    int nsize_buf;
    int num_ele_buf;
    int num_each_ele;
    unsigned int *ie_buf;
};


struct kemoview_gl_texure{
    int texure_width;
    int texure_height;
    int texure_npix;
    unsigned char  *texure_rgba;
};


/* prototypes */
long prod_padding_4096chars(long num_bytes);

struct gl_strided_buffer * init_strided_buffer(long num_points);
void alloc_strided_buffer(struct gl_strided_buffer *strided_buf);
void resize_strided_buffer(struct gl_strided_buffer *strided_buf);
void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf);

void set_buffer_address_4_patch(long num_points, struct gl_strided_buffer *strided_buf);
void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf);

void set_zero_stride_buffer(long inum, struct gl_strided_buffer *strided_buf);
void set_node_stride_buffer(long inum, struct gl_strided_buffer *strided_buf);
void select_strided_buffer(long inum, struct gl_strided_buffer *strided_buf);


struct gl_index_buffer * alloc_gl_index_buffer(int numele, int nnod_4_ele);
void dealloc_gl_index_buffer(struct gl_index_buffer * alloc_gl_index_buffer);


struct kemoview_gl_texure * alloc_kemoview_gl_texure(void);
void alloc_draw_psf_texture(struct kemoview_gl_texure *psf_texure);
void dealloc_draw_psf_texture(struct kemoview_gl_texure *psf_texure);
void dealloc_kemoview_gl_texure(struct kemoview_gl_texure *psf_texure);

#endif /* m_vertex_buffer_h_ */
