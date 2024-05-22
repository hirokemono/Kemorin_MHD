/*
 *  set_primitives_to_gl_buffer.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */


#ifndef SET_PRIMITIVES_TO_GL_BUFFER_
#define SET_PRIMITIVES_TO_GL_BUFFER_

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"
#include "icosahedron_c.h"

long set_nodes_strided_buffer(long inod_in, long numnod, double *xyzw_nod,
                              double *norm_nod, double *color_nod, double *xy_txur,
                              struct gl_strided_buffer *strided_buf);
long set_patch_strided_buffer(long ipatch_in, double xyzw_tri[12],
                              double norm_tri[12], double color_tri[12],
                              struct gl_strided_buffer *strided_buf);
long set_line_strided_buffer(const long ist_line,
                             double xyzw_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf);
long set_cone_strided_buffer(const long ist_tube, int ncorner, double radius,
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf);
long set_tube_strided_buffer(const long ist_tube, int ncorner, double radius,
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf);
long set_icosahedron_strided_buffer(long ist_ico, double node_diam,
                                    double xyzw_draw[4], double f_color[4],
                                    struct gl_strided_buffer *strided_buf);

long set_patch_textur_to_buf(long ist_texture, double xy_txur[6],
                             struct gl_strided_buffer *strided_buf);

#endif /*  SET_PRIMITIVES_TO_GL_BUFFER_ */
