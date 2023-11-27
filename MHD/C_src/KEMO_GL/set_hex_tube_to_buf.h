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

/* prototypes */

void copy_hex_tube_pp(int hex_tube[12][3]);
void copy_hex_tube_pn(int hex_tube[12][3]);
void copy_hex_tube_np(int hex_tube[12][3]);
void copy_hex_tube_nn(int hex_tube[12][3]);

void hex_ring(double edge_dir[3], double edge_norm[3], double norm_hex[18]);
void hex_ring_4_edge(double norms_hex[36], double dir_edge[6], double nrm_edge[6]);

void set_each_tube_data(double xx_tube[18], double norm_tube[18], double color_tube[24],
						int hex_tube[2][3], double norms_hex[36], double radius,
						double xyz_edge[6], double color_edge[8]);

void interpolate_on_edge(double xyz_mid[3], double dir_mid[3], double nrm_mid[3], 
						 const double xyz1[3], const double xyz2[3], 
						 const double nrm1[3], const double nrm2[3],
						 const double dat1, const double dat2, const double v_line);
int find_isoline_on_triangle(const double xyz_tri[9], 
							 const double d_tri[3], const double v_line);
int set_isoline_on_triangle(double xyz_line[6], double dir_line[6], double nrm_line[6], 
							const double xyz_tri[9], const double nrm_tri[9],
							const double d_tri[3], const double v_line);

int add_line_tube_patch_num(int ipatch_in);
int append_line_tube_to_buf(int ipatch_in, int hex_tube[12][3], double radius, 
							double color_edge[8], double xyz_edge[6], 
							double dir_edge[6], double nrm_edge[6], 
							struct gl_strided_buffer *strided_buf);

/* SET_HEX_TUBE_TO_BUF_ */
#endif

