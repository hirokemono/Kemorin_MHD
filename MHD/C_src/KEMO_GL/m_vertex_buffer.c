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

static long prod_padding_1024floats(long num_nod_buf, int ncomp_buf){
    long nsize = 4 * num_nod_buf * ncomp_buf;
    return prod_padding_4096chars(nsize);
};

struct gl_strided_buffer * init_strided_buffer(long num_points){
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

void set_buffer_address_4_patch(long num_points, struct gl_strided_buffer *strided_buf){
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


void set_zero_stride_buffer(long inum, struct gl_strided_buffer *strided_buf){
    strided_buf->x_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_xyz*strided_buf->num_nod_buf];
    strided_buf->d_draw = &strided_buf->v_buf[  inum + strided_buf->ist_data*strided_buf->num_nod_buf];
    strided_buf->c_draw = &strided_buf->v_buf[4*inum + strided_buf->ist_csurf*strided_buf->num_nod_buf];
    strided_buf->n_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_norm*strided_buf->num_nod_buf];
    strided_buf->x_txur = &strided_buf->v_buf[2*inum + strided_buf->ist_tex*strided_buf->num_nod_buf];
    return;
};

void set_node_stride_buffer(long inum, struct gl_strided_buffer *strided_buf){
    strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_xyz];
    strided_buf->d_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_data];
    strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_csurf];
    strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_norm];
    strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_tex];
    return;
};

void select_strided_buffer(long inum, struct gl_strided_buffer *strided_buf){
    if(strided_buf->istride == 0){
        set_zero_stride_buffer(inum, strided_buf);
    } else {
        set_node_stride_buffer(inum, strided_buf);
    };
}


struct gl_index_buffer * alloc_gl_index_buffer(int numele, int nnod_4_ele){
    struct gl_index_buffer *index_buf;
    if((index_buf = (struct gl_index_buffer *) malloc(sizeof(struct gl_index_buffer))) == NULL) {
        printf("malloc error in gl_index_buffer\n");
        exit(0);
    }
    index_buf->num_ele_buf =  numele;
    index_buf->num_each_ele = nnod_4_ele;
    index_buf->nsize_buf =    numele * nnod_4_ele;
    
    if((index_buf->ie_buf = (unsigned int *) malloc(index_buf->nsize_buf * sizeof(unsigned int))) == NULL){
        printf("malloc error in index_buf\n");
        exit(0);
    }
    return index_buf;
}

void dealloc_gl_index_buffer(struct gl_index_buffer *index_buf){
    if(index_buf->nsize_buf > 0) free(index_buf->ie_buf);
    free(index_buf);
    return;
};


struct gl_texure_image * alloc_kemoview_gl_texure(void){
    struct gl_texure_image *kemo_texure = (struct gl_texure_image *) malloc(sizeof(struct gl_texure_image));
    if (kemo_texure == NULL) {
        printf("Allocation failed for kemo_texure \n");
        exit(1);
    }
    kemo_texure->texure_npix =  0;
    kemo_texure->nipxel_xy[0] = 0;
    kemo_texure->nipxel_xy[1] = 0;
    return kemo_texure;
}

void alloc_draw_psf_texture(const int npixel_x, const int npixel_y,
                            struct gl_texure_image *kemo_texure){
    kemo_texure->nipxel_xy[0] = npixel_x;
    kemo_texure->nipxel_xy[1] = npixel_y;
    kemo_texure->texure_npix = kemo_texure->nipxel_xy[0] * kemo_texure->nipxel_xy[1];
    kemo_texure->texure_rgba = (unsigned char *) malloc( (4*kemo_texure->texure_npix) * sizeof(unsigned char));
    if ((kemo_texure->texure_rgba) == NULL) {
        printf("Allocation failed for kemo_texure->texure_rgba \n");
        exit(2);
    }
    return;
}


void dealloc_draw_psf_texture(struct gl_texure_image *kemo_texure){
    if(kemo_texure->texure_npix > 0) {free(kemo_texure->texure_rgba);};
    kemo_texure->texure_npix = 0;
    return;
}

void dealloc_kemoview_gl_texure(struct gl_texure_image *kemo_texure){
    dealloc_draw_psf_texture(kemo_texure);
    free(kemo_texure);
    return;
}

void clear_kemoview_gl_texure(struct gl_texure_image *kemo_texure){
    int i;
    for(i=0;i<kemo_texure->texure_npix;i++){
        kemo_texure->texure_rgba[4*i  ] =  0;
        kemo_texure->texure_rgba[4*i+1] =  0;
        kemo_texure->texure_rgba[4*i+2] =  0;
        kemo_texure->texure_rgba[4*i+3] =  0;
    };
    return;
};
