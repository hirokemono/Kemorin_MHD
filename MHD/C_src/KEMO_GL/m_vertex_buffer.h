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

#include "skip_comment_c.h"

struct gl_local_buffer_address{
    long igl_xyzw;
    long igl_txur;
    long igl_norm;
    long igl_color;
    long igl_data;
};


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
};

struct gl_index_buffer{
    long nsize_buf;
    long ntot_vertex;
    long num_ele_buf;
    int num_each_ele;
    unsigned int *ie_buf;
};


struct gl_texure_image{
    int nipxel_xy[2];
    int texure_npix;
    unsigned char  *texure_rgba;
};

struct gl_textbox_buffer{
    float text_opacity;
    int len_text;
    char *texts;

    struct gl_strided_buffer *vertex;
    struct gl_texure_image *image;
};

/* prototypes */
long prod_padding_4096chars(long num_bytes);

struct gl_strided_buffer * init_strided_buffer(long num_points);
void alloc_strided_buffer(struct gl_strided_buffer *strided_buf);
void resize_strided_buffer(struct gl_strided_buffer *strided_buf);
void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf);

void set_buffer_address_4_patch(long num_points, struct gl_strided_buffer *strided_buf);
void set_buffer_address_4_colormap(long num_points, struct gl_strided_buffer *strided_buf);

void set_zero_stride_buffer(long inum, struct gl_strided_buffer *strided_buf,
                            struct gl_local_buffer_address *point_buf);
void set_node_stride_buffer(long inum, struct gl_strided_buffer *strided_buf,
                            struct gl_local_buffer_address *point_buf);
void select_strided_buffer(long inum, struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf);


struct gl_index_buffer * init_gl_index_buffer(long numele, int nnod_4_ele);
void dealloc_gl_index_buffer(struct gl_index_buffer * index_buf);
void resize_gl_index_buffer(long numele, int nnod_4_ele,
                            struct gl_index_buffer *index_buf);


struct gl_texure_image * alloc_kemoview_gl_texure(void);
void alloc_draw_psf_texture(const int npixel_x, const int npixel_y,
                            struct gl_texure_image *kemo_texure);
void dealloc_draw_psf_texture(struct gl_texure_image *kemo_texure);
void dealloc_kemoview_gl_texure(struct gl_texure_image *kemo_texure);
void clear_kemoview_gl_texure(struct gl_texure_image *kemo_texure);

struct gl_textbox_buffer * alloc_line_text_image(const int npix_x, const int npix_y,
                                                 const long n_vertex, const int len_text);
void dealloc_line_text_image(struct gl_textbox_buffer *l_txt_img);

#endif /* m_vertex_buffer_h_ */
