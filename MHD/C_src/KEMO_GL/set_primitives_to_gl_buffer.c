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
    struct gl_local_buffer_address point_buf;
    for(long inod = 0; inod < numnod; inod++) {
        set_node_stride_buffer((inod_in+inod), strided_buf, &point_buf);
        for(int nd=0;nd<4;nd++){
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
    
    long ipatch = ITHREE*ipatch_in;
    for (k = 0; k < ITHREE; k++) {
        set_node_stride_buffer((ipatch+k), strided_buf, &point_buf);
        for(int nd=0;nd<4;nd++){
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

long set_cone_strided_buffer(const long ist_cone, int ncorner, double radius, 
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
    double norm_line[8];
    long npatch_wall;
	long k, nd;
	
    find_normal_on_line(&norm_line[0], &dir_line[0]);
    find_normal_on_line(&norm_line[4], &dir_line[4]);
    npatch_wall = set_cone_vertex(ncorner, radius,
                                  xyzw_line, dir_line,
                                  norm_line, color_line,
                                  xyzw, norm, col);
    long ist_vertex = ITHREE * npatch_wall * ist_cone;
	for (k=0; k<ITHREE*npatch_wall; k++) {
        set_node_stride_buffer((ist_vertex+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
	};
    return (ist_cone + 1);
}

long set_tube_strided_buffer(const long ist_tube, int ncorner, double radius, 
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
    double norm_line[8];
    long npatch_wall;
	long k, nd;
	
	find_normal_on_line(&norm_line[0], &dir_line[0]);
	find_normal_on_line(&norm_line[4], &dir_line[4]);
    npatch_wall = set_tube_vertex(ncorner, radius,
                                  xyzw_line, dir_line,
                                  norm_line, color_line,
                                  xyzw, norm, col);
    long ist_vertex = ITHREE * npatch_wall * ist_tube;
	for (k=0; k<ITHREE*npatch_wall; k++) {
        set_node_stride_buffer((ist_vertex+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
	};
    return (ist_tube + 1);
}

long set_icosahedron_strided_buffer(long ist_ico, double node_diam,
                                    double xyzw_draw[4], double f_color[4],
                                    struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    double xyzw_patch[240], norm_patch[240];
    long icou, nd;
    
    long ntri_ico = set_icosahedron_patch(node_diam, xyzw_draw,
                                          xyzw_patch, norm_patch);
    long ist_tri = ITHREE * ntri_ico * ist_ico;
    for(icou=0; icou<ITHREE*ntri_ico; icou++){
        set_node_stride_buffer((ist_tri+icou), strided_buf, &point_buf);
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
