/*
 *  set_hex_tube_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_hex_tube_to_buf.h"

static int hex_tube_pp[12][3] = {{0, 6,1}, {1, 6, 7},
								 {1, 7,2}, {2, 7, 8},
								 {2, 8,3}, {3, 8, 9},
								 {3, 9,4}, {4, 9,10},
								 {4,10,5}, {5,10,11},
								 {5,11,0}, {0,11, 6}};
static int hex_tube_pn[12][3] = {{0, 9,1}, {1, 9, 8},
								 {1, 8,2}, {2, 8, 7},
								 {2, 7,3}, {3, 7, 6},
								 {3, 6,4}, {4, 6,11},
								 {4,11,5}, {5,11,10},
								 {5,10,0}, {0,10, 9}};
static int hex_tube_nn[12][3] = {{3, 9,2}, {2, 9, 8},
								 {2, 8,1}, {1, 8, 7},
								 {1, 7,0}, {0, 7, 6},
								 {0, 6,5}, {5, 6,11},
								 {5,11,4}, {4,11,10},
								 {4,10,3}, {3,10, 9}};
static int hex_tube_np[12][3] = {{3, 6,2}, {2, 6, 7},
								 {2, 7,1}, {1, 7, 8},
								 {1, 8,0}, {0, 8, 9},
								 {0, 9,5}, {5, 9,10},
								 {5,10,4}, {4,10,11},
								 {4,11,3}, {3,11, 6}};
/*
static int hex_cap_n[ 6][3] = {{0,6,1},
							   {1,6,2},
							   {2,6,3},
							   {3,6,4},
							   {4,6,5},
							   {5,6,0}};
static int hex_cap_p[ 6][3] = {{3,6,2},
							   {2,6,1},
							   {1,6,0},
							   {0,6,5},
							   {5,6,4},
							   {4,6,3}};
*/

void copy_hex_tube_pp(int hex_tube[12][3]){
	int i, k;
	for(i=0;i<12;i++){
		for(k=0;k<3;k++){hex_tube[i][k] = hex_tube_pp[i][k];};
	};
	return;
};

void copy_hex_tube_pn(int hex_tube[12][3]){
	int i, k;
	for(i=0;i<12;i++){
		for(k=0;k<3;k++){hex_tube[i][k] = hex_tube_pn[i][k];};
	};
	return;
};

void copy_hex_tube_np(int hex_tube[12][3]){
	int i, k;
	for(i=0;i<12;i++){
		for(k=0;k<3;k++){hex_tube[i][k] = hex_tube_np[i][k];};
	};
	return;
};

void copy_hex_tube_nn(int hex_tube[12][3]){
	int i, k;
	for(i=0;i<12;i++){
		for(k=0;k<3;k++){hex_tube[i][k] = hex_tube_nn[i][k];};
	};
	return;
};

void hex_ring(double edge_dir[4], double edge_norm[4], double norm_hex[24]){
	int nd;
	double asqrt3 = sqrt(3.0) / 3.0;
	
	for(nd=0;nd<3;nd++){
		norm_hex[   nd] =        edge_dir[nd];
		norm_hex[ 4+nd] =  0.5 * edge_dir[nd] + asqrt3 * edge_norm[nd];
		norm_hex[ 8+nd] = -0.5 * edge_dir[nd] + asqrt3 * edge_norm[nd];
		norm_hex[12+nd] =      - edge_dir[nd];
		norm_hex[16+nd] = -0.5 * edge_dir[nd] - asqrt3 * edge_norm[nd];
		norm_hex[20+nd] =  0.5 * edge_dir[nd] - asqrt3 * edge_norm[nd];
	};
    for(nd=0;nd<6;nd++){
        norm_hex[4*nd+3] = 1.0;
    }
	return;
}

void hex_ring_4_edge(double norms_hex[48], double dir_edge[8], double norm_edge[8]){
	hex_ring(&dir_edge[0], &norm_edge[0], &norms_hex[ 0]);
	hex_ring(&dir_edge[4], &norm_edge[4], &norms_hex[24]);
	return;
}

void set_each_tube_data(double xyzw_tube[24], double norm_tube[24], double color_tube[24],
						int hex_tube[2][3], double norms_hex[48], double radius,
						double xyzw_edge[8], double color_edge[8]){
    int nd;
	int i1 = hex_tube[0][0];
	int i2 = hex_tube[0][1];
	int i3 = hex_tube[0][2];
	int j1 = hex_tube[1][0];
	int j2 = hex_tube[1][1];
	int j3 = hex_tube[1][2];
	
    for(nd=0;nd<4;nd++){
        color_tube[   nd] = color_edge[  nd];
        color_tube[ 4+nd] = color_edge[4+nd];
        color_tube[ 8+nd] = color_edge[  nd];
        color_tube[12+nd] = color_edge[  nd];
        color_tube[16+nd] = color_edge[4+nd];
        color_tube[20+nd] = color_edge[4+nd];
    };
	for(nd=0;nd<4;nd++){
		norm_tube[   nd] = norms_hex[4*i1+nd];
		norm_tube[ 4+nd] = norms_hex[4*i2+nd];
		norm_tube[ 8+nd] = norms_hex[4*i3+nd];
		norm_tube[12+nd] = norms_hex[4*j1+nd];
		norm_tube[16+nd] = norms_hex[4*j2+nd];
		norm_tube[20+nd] = norms_hex[4*j3+nd];
	};
	for(nd=0;nd<3;nd++){
        xyzw_tube[   nd] = xyzw_edge[  nd] + radius * norm_tube[   nd];
        xyzw_tube[ 4+nd] = xyzw_edge[4+nd] + radius * norm_tube[ 4+nd];
        xyzw_tube[ 8+nd] = xyzw_edge[  nd] + radius * norm_tube[ 8+nd];
        xyzw_tube[12+nd] = xyzw_edge[  nd] + radius * norm_tube[12+nd];
        xyzw_tube[16+nd] = xyzw_edge[4+nd] + radius * norm_tube[16+nd];
        xyzw_tube[20+nd] = xyzw_edge[4+nd] + radius * norm_tube[20+nd];
	};
    for(nd=0;nd<6;nd++){
        xyzw_tube[4*nd+3] = 1.0;
    };
	return;
};

void interpolate_on_edge(double xyzw_mid[4], double dir_mid[4], double norm_mid[4], 
						 const double xyzw1[4], const double xyzw2[4], 
						 const double norm1[4], const double norm2[4],
						 const double dat1, const double dat2, const double v_line){
	int nd;
	double coef = (dat2 - v_line) / (dat2 - dat1);
	for(nd=0; nd<4; nd++){
		dir_mid[nd] =  xyzw2[nd] - xyzw1[nd];
        xyzw_mid[nd] = coef * xyzw1[nd] + (1.0 - coef) * xyzw2[nd];
        norm_mid[nd] = coef * norm1[nd] + (1.0 - coef) * norm2[nd];
	};
	coef = sqrt(dir_mid[0]*dir_mid[0] + dir_mid[1]*dir_mid[1] + dir_mid[2]*dir_mid[2]);
	for(nd=0; nd<4; nd++){
		dir_mid[nd] = dir_mid[nd] / coef;
	};
    xyzw_mid[3] = 1.0;
    dir_mid[3] =  1.0;
    norm_mid[3] = 1.0;
	return;
};

int find_isoline_on_triangle(const double d_tri[3], const double v_line){
	double sig[3];
	int k1, i1, i2, i3;
	
	int idraw = 0;
	for (k1 = 0; k1 < 3; k1++) {
		i1 = (k1  )%3;
		i2 = (k1+1)%3;
		i3 = (k1+2)%3;
		
		sig[0] = (d_tri[i2] - v_line) * (d_tri[i3] - v_line);
		sig[1] = (d_tri[i3] - v_line) * (d_tri[i1] - v_line);
		sig[2] = (d_tri[i1] - v_line) * (d_tri[i2] - v_line);
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			idraw = 1;
			break;
		}
	}
	return idraw;
};

int set_isoline_on_triangle(double xyzw_line[8], double dir_line[8], double norm_line[8], 
							const double xyzw_tri[12], const double norm_tri[12],
							const double d_tri[3], const double v_line){
	double sig[3];
	int k1, nd, i1, i2, i3;
	
	int idraw = 0;
	for (k1 = 0; k1 < 3; k1++) {
		i1 = (k1  )%3;
		i2 = (k1+1)%3;
		i3 = (k1+2)%3;
		
		sig[0] = (d_tri[i2] - v_line) * (d_tri[i3] - v_line);
		sig[1] = (d_tri[i3] - v_line) * (d_tri[i1] - v_line);
		sig[2] = (d_tri[i1] - v_line) * (d_tri[i2] - v_line);
		
		if ( (sig[0]==ZERO) && (sig[1]==ZERO) && (sig[2]<ZERO) ){
			interpolate_on_edge(&xyzw_line[0], &dir_line[0], &norm_line[0], 
								&xyzw_tri[4*i1], &xyzw_tri[4*i2], 
								&norm_tri[4*i1], &norm_tri[4*i2],
								d_tri[i1], d_tri[i2], v_line);
			for(nd=0; nd<3; nd++){
                xyzw_line[4+nd] = xyzw_tri[4*i3+nd];
				dir_line[4+nd] = -dir_line[nd];
                norm_line[4+nd] = xyzw_tri[4*i3+nd];
			};
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_on_edge(&xyzw_line[0], &dir_line[0], &norm_line[0], 
								&xyzw_tri[4*i1], &xyzw_tri[4*i2], 
								&norm_tri[4*i1], &norm_tri[4*i2],
								d_tri[i1], d_tri[i2], v_line);
			interpolate_on_edge(&xyzw_line[4], &dir_line[4], &norm_line[4], 
								&xyzw_tri[4*i3], &xyzw_tri[4*i2], 
								&norm_tri[4*i3], &norm_tri[4*i2],
								d_tri[i3], d_tri[i2], v_line);
			idraw = 1;
			break;
		}
	}
    xyzw_line[ 3] = 1.0;
    xyzw_line[ 7] = 1.0;
	if(idraw == 0) return idraw;
	
	if(v_line < 0.0){
		for(nd=0; nd<4; nd++){
            xyzw_line[ 4+nd] =   0.5*(xyzw_line[  nd] + xyzw_line[4+nd]);
		};
	};
	return idraw;
};


int add_line_tube_patch_num(int ipatch_in){return ipatch_in + 12;};

long append_line_tube_to_buf(const long ipatch_in, 
                             int hex_tube[12][3], double radius, 
							 double color_edge[8], double xyzw_edge[8], 
							 double dir_edge[8], double norm_edge[8], 
							 struct gl_strided_buffer *strided_buf,
                             struct gl_local_buffer_address *point_buf){
	long ipatch = ipatch_in;
	long i, k, nd;
	double xyzw_tube[24];
	double norm_tube[24];
	double color_tube[24];
	
	double norms_hex[48];
	
	hex_ring_4_edge(norms_hex, dir_edge, norm_edge);
	for(i=0;i<6;i++){
		set_each_tube_data(xyzw_tube, norm_tube, color_tube, 
						   &hex_tube[2*i], norms_hex, radius, xyzw_edge, color_edge);
		for(k=0;k<6;k++){
            set_node_stride_buffer((3*ipatch+6*i+k), strided_buf, point_buf);
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf->igl_xyzw] =  (float) xyzw_tube[4*k+nd];};
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf->igl_color] = (float) color_tube[4*k+nd];};
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf->igl_norm] =  (float) norm_tube[4*k+nd];};
		};
	};
	return ipatch + 12;
};
