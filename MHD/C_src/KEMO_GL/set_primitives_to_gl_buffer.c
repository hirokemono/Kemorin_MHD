/*
 *  set_primitives_to_gl_buffer.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_primitives_to_gl_buffer.h"

#define ARCPI 0.318309886


long set_nodes_strided_buffer(long inod_in, long numnod, double *xyzw_nod,
                              double *norm_nod, double *color_nod, double *xy_txur,
                              struct gl_strided_buffer *strided_buf){
    int nd;
    long inod;
    struct gl_local_buffer_address point_buf;
    for(inod = 0; inod < numnod; inod++) {
        set_node_stride_buffer((inod_in+inod), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] =  xyzw_nod[4*inod+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = color_nod[4*inod+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] =  norm_nod[4*inod+nd];
        };
        strided_buf->v_buf[  point_buf.igl_txur] = xy_txur[2*inod+1] * ARCPI * HALF;
        strided_buf->v_buf[1+point_buf.igl_txur] = ONE - xy_txur[2*inod  ] * ARCPI;
    };
    return (inod_in + numnod);
}

long set_patch_strided_buffer(long ipatch_in, double xyzw_tri[12],
                              double norm_tri[12], double color_tri[12],
                              struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long k;
    int nd;
    
    long ipatch = ITHREE*ipatch_in;
    for (k = 0; k < ITHREE; k++) {
        set_node_stride_buffer((ipatch+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] =  xyzw_tri[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = color_tri[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] =  norm_tri[4*k+nd];
        };
    };
    return (ipatch_in + 1);
}

long set_line_strided_buffer(const long ist_line,
                             double xyzw_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    const long nvertex_line = 2;
    double norm_line[4] = {0.0, 0.0, 1.0, 1.0};
	long k, nd;
	
    long ist_vertex = nvertex_line * ist_line;
	for (k=0; k<nvertex_line; k++) {
        set_node_stride_buffer((ist_vertex+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] =  xyzw_line[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] =  norm_line[nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = color_line[4*k+nd];
        };
	};
    return (ist_line + 1);
}

long set_cone_node_index_buffer(const long ist_cone, int ncorner, double radius,
                                double xyzw_line[8], double dir_line[8], double color_line[8],
                                struct gl_strided_buffer *strided_buf,
                                struct gl_index_buffer *index_buf){
    struct gl_local_buffer_address point_buf;
    double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
    double norm_line[8];
    long npatch_wall;
    long k, nd;
    
    long n_patch = ITHREE * num_cone_patch(ncorner);
    long ist_nod = num_cone_node(ncorner) * ist_cone;
    long ist_index = n_patch * ist_cone;
    find_normal_on_line(&norm_line[0], &dir_line[0]);
    find_normal_on_line(&norm_line[4], &dir_line[4]);
    npatch_wall = set_cone_node_index(ncorner, radius,
                                      xyzw_line, dir_line,
                                      norm_line, color_line,
                                      xyzw, norm, col,
                                      &index_buf->ie_buf[ist_index]);
    
    for(k=0; k<n_patch; k++){
        index_buf->ie_buf[ist_index+k]
            = index_buf->ie_buf[ist_index+k] + ist_nod;
    };

    for (k=0; k<num_cone_node(ncorner); k++) {
        set_node_stride_buffer((ist_nod+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
    };
    return (ist_cone+1);
}

long set_tube_node_index_buffer(const long ist_tube, int ncorner, double radius,
                                double xyzw_line[8], double dir_line[8], double color_line[8],
                                struct gl_strided_buffer *strided_buf,
                                struct gl_index_buffer *index_buf){
    struct gl_local_buffer_address point_buf;
    double xyzw[8*(ncorner+1)];
    double norm[8*(ncorner+1)];
    double col[8*(ncorner+1)];
    double norm_line[8];
    long npatch_wall;
    long k, nd;
    
//    color_line[3] = 1.0;
//    color_line[7] = 1.0;
    find_normal_on_line(&norm_line[0], &dir_line[0]);
    find_normal_on_line(&norm_line[4], &dir_line[4]);
    
    long n_patch = ITHREE * num_tube_patch(ncorner);
    long ist_node =  num_tube_node(ncorner) * ist_tube;
    long ist_index = n_patch * ist_tube;
    npatch_wall = set_tube_node_index(ncorner, radius,
                                      xyzw_line, dir_line,
                                      norm_line, color_line,
                                      xyzw, norm, col,
                                      &index_buf->ie_buf[ist_index]);

    for(k=0; k<n_patch; k++){
        index_buf->ie_buf[ist_index+k]
            = index_buf->ie_buf[ist_index+k] + ist_node;
    };
    for(k=0; k<num_tube_node(ncorner); k++) {
        set_node_stride_buffer((ist_node+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
    };
    return (ist_tube + 1);
}


long set_icosahedron_node_index_buffer(long ist_ico, double node_diam,
                                       double xyzw_draw[4], double color_draw[4],
                                       struct gl_strided_buffer *strided_buf,
                                       struct gl_index_buffer *index_buf){
    struct gl_local_buffer_address point_buf;
    double xyzw_patch[4*12], norm_patch[4*12];
    double f_color[4];
    long icou, nd;
    
    long n_vertex =  ITHREE * num_icosahedron_patch();
    long ist_index = n_vertex * ist_ico;
    long ist_node = num_icosahedron_node() * ist_ico;
    long nnod_ico = set_icosahedron_node_index(node_diam, xyzw_draw,
                                               xyzw_patch, norm_patch,
                                               &index_buf->ie_buf[ist_index]);
    for(nd=0;nd<4;nd++){f_color[nd] = color_draw[nd];};
    f_color[3] = 1.0;

    for(icou=0; icou<n_vertex; icou++){
        index_buf->ie_buf[ist_index+icou]
            = index_buf->ie_buf[ist_index+icou] + (unsigned int) ist_node;
    };
    for(icou=0; icou<num_icosahedron_node(); icou++){
        set_node_stride_buffer((icou+ist_node), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] =  xyzw_patch[4*icou+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] =  norm_patch[4*icou+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];
        };
    };
    return (ist_ico+1);
}

long set_icosahedron_single_color_buffer(long ist_ico, double node_diam,
                                         double xyzw_draw[4], double f_color[4],
                                         struct gl_strided_buffer *strided_buf,
                                         struct gl_index_buffer *index_buf){
    struct gl_local_buffer_address point_buf;
    double xyzw_patch[240], norm_patch[240];
    long icou, nd;
    
    long n_vertex =  ITHREE * num_icosahedron_patch();
    long ist_index = n_vertex * ist_ico;
    long ist_node = num_icosahedron_node() * ist_ico;
    long ntri_ico = set_icosahedron_node_index(node_diam, xyzw_draw,
                                               xyzw_patch, norm_patch,
                                               &index_buf->ie_buf[ist_index]);

    for(icou=0; icou<n_vertex; icou++){
        index_buf->ie_buf[ist_index+icou]
            = index_buf->ie_buf[ist_index+icou] + (unsigned int) ist_node;
    };

    f_color[3] = 1.0;
    for(icou=0; icou<num_icosahedron_node(); icou++){
        set_node_stride_buffer((ist_node+icou), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw_patch[4*icou+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm_patch[4*icou+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];
        };
    };
    return (ist_ico+1);
}


long set_patch_textur_to_buf(long ist_texture, double xy_txur[6],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    
    long ipatch = ITHREE*ist_texture;
    for (int k=0; k<ITHREE; k++) {
        set_node_stride_buffer((ipatch+k), strided_buf, &point_buf);
        strided_buf->v_buf[point_buf.igl_txur  ] = xy_txur[2*k  ];
        strided_buf->v_buf[point_buf.igl_txur+1] = xy_txur[2*k+1];
    };
    return (ist_texture + 1);
}
