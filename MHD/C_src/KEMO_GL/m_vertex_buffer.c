/*
//  m_vertex_buffer.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "m_vertex_buffer.h"

long prod_padding_4096chars(long num_bytes){
    long nsize = 1 + num_bytes / 4096;
    return (4096 * nsize);
};

static long prod_padding_1024floats(int num_nod_buf, int ncomp_buf){
    long nsize = 4 * num_nod_buf * ncomp_buf;
    return prod_padding_4096chars(nsize);
};

struct gl_strided_buffer * init_strided_buffer(int num_points){
    struct gl_strided_buffer *strided_buf;
    if((strided_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer))) == NULL){
        printf("malloc error for gl_strided_buffer\n");
        exit(0);
    };
    
    set_buffer_address_4_patch(num_points, strided_buf);
    alloc_strided_buffer(strided_buf);
    return strided_buf;
};

void alloc_strided_buffer(struct gl_strided_buffer *strided_buf){
    strided_buf->nsize_buf = prod_padding_1024floats(strided_buf->num_nod_buf, strided_buf->ncomp_buf);
    if((strided_buf->v_buf = (float *) malloc(strided_buf->nsize_buf*sizeof(float))) == NULL){
        printf("malloc error for strided_buf->v_buf\n");
        exit(0);
    };
};

void resize_strided_buffer(struct gl_strided_buffer *strided_buf){
    long nsize = prod_padding_1024floats(strided_buf->num_nod_buf, strided_buf->ncomp_buf);
    if(nsize <= strided_buf->nsize_buf) return;
    
    float *tmp = NULL;
    strided_buf->nsize_buf = nsize;
    tmp = (float *) realloc(strided_buf->v_buf, strided_buf->nsize_buf*sizeof(float));
    if(tmp == NULL){
        printf("reallocation error for strided_buf->v_buf\n");
        exit(-1);
    } else {
        strided_buf->v_buf = tmp;
    };
    
    return;
};

void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf){
    free(strided_buf->v_buf);
    free(strided_buf);
    return;
};

void set_buffer_address_4_patch(int num_points, struct gl_strided_buffer *strided_buf){
    strided_buf->ist_xyz =    0;
    strided_buf->ist_csurf =  4;
    strided_buf->ist_norm =   8;
    strided_buf->ist_tex =   12;
    strided_buf->ist_data =  14;

    strided_buf->ncomp_buf = 16;
    strided_buf->num_nod_buf = num_points;
    strided_buf->istride = sizeof(float) * strided_buf->ncomp_buf;
    return;
};

void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf){
    strided_buf->ist_xyz =    0;
    strided_buf->ist_csurf =  2;
    strided_buf->ist_data =   6;
    
    strided_buf->ist_norm =  -1;
    strided_buf->ist_tex =   -1;

    strided_buf->ncomp_buf =  8;
    strided_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
    
    return;
};


void set_zero_stride_buffer(int inum, struct gl_strided_buffer *strided_buf){
    strided_buf->x_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_xyz*strided_buf->num_nod_buf];
    strided_buf->d_draw = &strided_buf->v_buf[inum +   strided_buf->ist_data*strided_buf->num_nod_buf];
    strided_buf->c_draw = &strided_buf->v_buf[4*inum + strided_buf->ist_csurf*strided_buf->num_nod_buf];
    strided_buf->n_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_norm*strided_buf->num_nod_buf];
    strided_buf->x_txur = &strided_buf->v_buf[2*inum + strided_buf->ist_tex*strided_buf->num_nod_buf];
    return;
};

void set_node_stride_buffer(int inum, struct gl_strided_buffer *strided_buf){
    strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_xyz];
    strided_buf->d_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_data];
    strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_csurf];
    strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_norm];
    strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_tex];
    return;
};

void select_strided_buffer(int inum, struct gl_strided_buffer *strided_buf){
    if(strided_buf->istride == 0){
        set_zero_stride_buffer(inum, strided_buf);
    } else {
        set_node_stride_buffer(inum, strided_buf);
    };
}

