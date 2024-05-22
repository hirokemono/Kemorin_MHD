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

void set_buffer_address_4_colormap(long num_points, struct gl_strided_buffer *strided_buf){
    strided_buf->ist_xyz =    0;
    strided_buf->ist_data =   4;
    strided_buf->ist_norm =   5;
    strided_buf->ist_tex =    8;
    strided_buf->ist_csurf =  8;

    strided_buf->ncomp_buf = 12;
    strided_buf->num_nod_buf = num_points;
    strided_buf->istride = sizeof(float) * strided_buf->ncomp_buf;
    return;
};

void set_zero_stride_buffer(long inum, struct gl_strided_buffer *strided_buf,
                            struct gl_local_buffer_address *point_buf){
    point_buf->igl_xyzw =  4*inum + strided_buf->ist_xyz *   strided_buf->num_nod_buf;
    point_buf->igl_color = 4*inum + strided_buf->ist_csurf * strided_buf->num_nod_buf;
    point_buf->igl_norm =  4*inum + strided_buf->ist_norm *  strided_buf->num_nod_buf;
    point_buf->igl_txur =  2*inum + strided_buf->ist_tex *   strided_buf->num_nod_buf;
    point_buf->igl_data =    inum + strided_buf->ist_data *  strided_buf->num_nod_buf;
    return;
};

void set_node_stride_buffer(long inum, struct gl_strided_buffer *strided_buf,
                            struct gl_local_buffer_address *point_buf){
    point_buf->igl_xyzw =  strided_buf->ncomp_buf * inum + strided_buf->ist_xyz;
    point_buf->igl_color = strided_buf->ncomp_buf * inum + strided_buf->ist_csurf;
    point_buf->igl_norm =  strided_buf->ncomp_buf * inum + strided_buf->ist_norm;
    point_buf->igl_txur =  strided_buf->ncomp_buf * inum + strided_buf->ist_tex;
    point_buf->igl_data =  strided_buf->ncomp_buf * inum + strided_buf->ist_data;
    return;
};

void select_strided_buffer(long inum, struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf){
    if(strided_buf->istride == 0){
        set_zero_stride_buffer(inum, strided_buf, point_buf);
    } else {
        set_node_stride_buffer(inum, strided_buf, point_buf);
    };
}


void alloc_gl_index_buffer(struct gl_index_buffer *index_buf){
    index_buf->nsize_buf = index_buf->ntot_vertex;
    if((index_buf->ie_buf = (unsigned int *) malloc(index_buf->nsize_buf * sizeof(unsigned int))) == NULL){
        printf("malloc error in index_buf\n");
        exit(0);
    }
    return;
}

struct gl_index_buffer * init_gl_index_buffer(long numele, int nnod_4_ele){
    struct gl_index_buffer *index_buf;
    if((index_buf = (struct gl_index_buffer *) malloc(sizeof(struct gl_index_buffer))) == NULL) {
        printf("malloc error in gl_index_buffer\n");
        exit(0);
    }
    index_buf->num_ele_buf =  numele;
    index_buf->num_each_ele = nnod_4_ele;
    index_buf->ntot_vertex = numele * nnod_4_ele;
    alloc_gl_index_buffer(index_buf);
    return index_buf;
}

void dealloc_gl_index_buffer(struct gl_index_buffer *index_buf){
    if(index_buf->nsize_buf > 0) free(index_buf->ie_buf);
    free(index_buf);
    return;
};

void resize_gl_index_buffer(long numele, int nnod_4_ele,
                            struct gl_index_buffer *index_buf){
    index_buf->num_ele_buf =  numele;
    index_buf->num_each_ele = nnod_4_ele;
    index_buf->ntot_vertex = numele * nnod_4_ele;
    if(index_buf->ntot_vertex <= index_buf->nsize_buf) return;
    
    free(index_buf->ie_buf);
    alloc_gl_index_buffer(index_buf);
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


struct gl_textbox_buffer * alloc_line_text_image(const int npix_x, const int npix_y,
                                                 const long n_vertex, const int len_text)
{
    struct gl_textbox_buffer *l_txt_img;
    if((l_txt_img = (struct gl_textbox_buffer *) malloc(sizeof(struct gl_textbox_buffer))) == NULL){
        printf("malloc error for gl_textbox_buffer\n");
        exit(0);
    }
    l_txt_img->text_opacity = 0.0;
    l_txt_img->len_text =  len_text;
    l_txt_img->texts = alloc_string(len_text);

    l_txt_img->vertex = init_strided_buffer(n_vertex);
    
    l_txt_img->image = alloc_kemoview_gl_texure();
    alloc_draw_psf_texture(npix_x, npix_y, l_txt_img->image);
    return l_txt_img;
};

void dealloc_line_text_image(struct gl_textbox_buffer *l_txt_img)
{
    dealloc_strided_buffer(l_txt_img->vertex);
    dealloc_kemoview_gl_texure(l_txt_img->image);
    free(l_txt_img->texts);
    free(l_txt_img);
    return;
};
