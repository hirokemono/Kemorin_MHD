/*
 *  set_hex_tube_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef SET_HEX_TUBE_TO_BUF_
#define SET_HEX_TUBE_TO_BUF_

#include <math.h>
#include "calypso_param_c.h"
#include "m_vertex_buffer.h"
#include "t_psf_edge_connect_c.h"
#include "cal_surface_center_normal_c.h"
#include "icosahedron_c.h"

/* prototypes */

void interpolate_on_edge(double xyzw_mid[4], const double xyzw1[4], const double xyzw2[4],
						 const double dat1, const double dat2, const double v_line);
int find_isoline_on_triangle(const double d_tri[3], const double v_line);
int set_isoline_on_triangle(long iedge_itp[2], double xyzw_line[8],
                            long iele, const double xyzw_tri[12],
                            const double d_tri[3], const double v_line,
                            struct psf_edge_data_c *psf_edge);

long append_line_tube_to_buf(const long ist_line,
                             int ncorner, double radius, 
                             double color_edge[8],
                             double xyzw_edge[8], 
							 double dir_edge[8],
                             struct gl_strided_buffer *strided_buf);

/* SET_HEX_TUBE_TO_BUF_ */
#endif

