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

void hex_ring(double edge_dir[3], double edge_norm[3], double norm_hex[18]){
	int nd;
	double asqrt3 = sqrt(3.0) / 3.0;
	
	for(nd=0;nd<3;nd++){
		norm_hex[   nd] =        edge_dir[nd];
		norm_hex[ 3+nd] =  0.5 * edge_dir[nd] + asqrt3 * edge_norm[nd];
		norm_hex[ 6+nd] = -0.5 * edge_dir[nd] + asqrt3 * edge_norm[nd];
		norm_hex[ 9+nd] =      - edge_dir[nd];
		norm_hex[12+nd] = -0.5 * edge_dir[nd] - asqrt3 * edge_norm[nd];
		norm_hex[15+nd] =  0.5 * edge_dir[nd] - asqrt3 * edge_norm[nd];
	};
	return;
}

void hex_ring_4_edge(double norms_hex[36], double dir_edge[6], double nrm_edge[6]){
	hex_ring(&dir_edge[0], &nrm_edge[0], &norms_hex[ 0]);
	hex_ring(&dir_edge[3], &nrm_edge[3], &norms_hex[18]);
	return;
}

void set_each_tube_data(double xx_tube[18], double norm_tube[18], double color_tube[24],
						int hex_tube[2][3], double norms_hex[36], double radius,
						double xyz_edge[6], double color_edge[8]){
    int nd;
	int i1 = hex_tube[0][0];
	int i2 = hex_tube[0][1];
	int i3 = hex_tube[0][2];
	int j1 = hex_tube[1][0];
	int j2 = hex_tube[1][1];
	int j3 = hex_tube[1][2];
	
	for(nd=0;nd<3;nd++){
		norm_tube[   nd] = norms_hex[3*i1+nd];
		norm_tube[ 3+nd] = norms_hex[3*i2+nd];
		norm_tube[ 6+nd] = norms_hex[3*i3+nd];
		norm_tube[ 9+nd] = norms_hex[3*j1+nd];
		norm_tube[12+nd] = norms_hex[3*j2+nd];
		norm_tube[15+nd] = norms_hex[3*j3+nd];
	};
	for(nd=0;nd<3;nd++){
		xx_tube[   nd] = xyz_edge[  nd] + radius * norm_tube[   nd];
		xx_tube[ 3+nd] = xyz_edge[3+nd] + radius * norm_tube[ 3+nd];
		xx_tube[ 6+nd] = xyz_edge[  nd] + radius * norm_tube[ 6+nd];
		xx_tube[ 9+nd] = xyz_edge[  nd] + radius * norm_tube[ 9+nd];
		xx_tube[12+nd] = xyz_edge[3+nd] + radius * norm_tube[12+nd];
		xx_tube[15+nd] = xyz_edge[3+nd] + radius * norm_tube[15+nd];
	};
	for(nd=0;nd<4;nd++){
		color_tube[   nd] = color_edge[  nd];
		color_tube[ 4+nd] = color_edge[4+nd];
		color_tube[ 8+nd] = color_edge[  nd];
		color_tube[12+nd] = color_edge[  nd];
		color_tube[16+nd] = color_edge[4+nd];
		color_tube[20+nd] = color_edge[4+nd];
	};
	return;
};

void interpolate_on_edge(double xyz_mid[3], double dir_mid[3], double nrm_mid[3], 
						 const double xyz1[3], const double xyz2[3], 
						 const double nrm1[3], const double nrm2[3],
						 const double dat1, const double dat2, const double v_line){
	int nd;
	double coef = (dat2 - v_line) / (dat2 - dat1);
	for(nd=0; nd<3; nd++){
		dir_mid[nd] =  xyz2[nd] - xyz1[nd];
		xyz_mid[nd] = coef * xyz1[nd] + (1.0 - coef) * xyz2[nd];
		nrm_mid[nd] = coef * nrm1[nd] + (1.0 - coef) * nrm2[nd];
	};
	coef = sqrt(dir_mid[0]*dir_mid[0] + dir_mid[1]*dir_mid[1] + dir_mid[2]*dir_mid[2]);
	for(nd=0; nd<3; nd++){
		dir_mid[nd] = dir_mid[nd] / coef;
	};
	return;
};

int find_isoline_on_triangle(const double xyz_tri[9], 
							 const double d_tri[3], const double v_line){
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

int set_isoline_on_triangle(double xyz_line[6], double dir_line[6], double nrm_line[6], 
							const double xyz_tri[9], const double nrm_tri[9],
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
			interpolate_on_edge(&xyz_line[0], &dir_line[0], &nrm_line[0], 
								&xyz_tri[3*i1], &xyz_tri[3*i2], 
								&nrm_tri[3*i1], &nrm_tri[3*i2],
								d_tri[i1], d_tri[i2], v_line);
			for(nd=0; nd<3; nd++){
				xyz_line[3+nd] = xyz_tri[3*i3+nd];
				dir_line[3+nd] = -dir_line[nd];
				nrm_line[3+nd] = xyz_tri[3*i3+nd];
			};
			idraw = 1;
			break;
		}
		else if ( (sig[0]<ZERO) && (sig[2]<ZERO) ){
			interpolate_on_edge(&xyz_line[0], &dir_line[0], &nrm_line[0], 
								&xyz_tri[3*i1], &xyz_tri[3*i2], 
								&nrm_tri[3*i1], &nrm_tri[3*i2],
								d_tri[i1], d_tri[i2], v_line);
			interpolate_on_edge(&xyz_line[3], &dir_line[3], &nrm_line[3], 
								&xyz_tri[3*i3], &xyz_tri[3*i2], 
								&nrm_tri[3*i3], &nrm_tri[3*i2],
								d_tri[i3], d_tri[i2], v_line);
			idraw = 1;
			break;
		}
	}
	if(idraw == 0) return idraw;
	
	if(v_line < 0.0){
		for(nd=0; nd<3; nd++){
			xyz_line[ 3+nd] =   0.5*(xyz_line[  nd] + xyz_line[3+nd]);
		};
	};
	return idraw;
};


int add_line_tube_patch_num(int ipatch_in){return ipatch_in + 12;};

int append_line_tube_to_buf(int ipatch_in, int hex_tube[12][3], double radius, 
							 double color_edge[8], double xyz_edge[6], 
							 double dir_edge[6], double nrm_edge[6], 
							 struct gl_strided_buffer *strided_buf){
	int ipatch = ipatch_in;
	int i, k, nd;
	double xx_tube[18];
	double norm_tube[18];
	double color_tube[24];
	
	double norms_hex[36];
	
	hex_ring_4_edge(norms_hex, dir_edge, nrm_edge);
	for(i=0;i<6;i++){
		set_each_tube_data(xx_tube, norm_tube, color_tube, 
						   &hex_tube[2*i], norms_hex, radius, xyz_edge, color_edge);
		for(k=0;k<6;k++){
            set_node_stride_buffer((3*ipatch+6*i+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = (float) xx_tube[3*k+nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = (float) color_tube[4*k+nd];};
			for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = (float) norm_tube[3*k+nd];};
		};
	};
	return ipatch + 12;
};
