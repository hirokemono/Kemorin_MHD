/*
 *  set_each_isoline_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_each_isoline_to_buf.h"

static void copy_each_triangle_postion(long ie_viz[3], double **xx_viz, double **d_nod, int icomp,
									   double xyz_tri[9], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod][icomp];
		xyz_tri[3*k  ] = xx_viz[inod][0];
		xyz_tri[3*k+1] = xx_viz[inod][1];
		xyz_tri[3*k+2] = xx_viz[inod][2];
	};
	return;
};
static void copy_each_triangle_postion_norm(long ie_viz[3], double **xx_viz, double **norm_nod,
											double **d_nod, int icomp,
											double xyz_tri[9], double nrm_tri[9], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod][icomp];
		xyz_tri[3*k  ] = xx_viz[inod][0];
		xyz_tri[3*k+1] = xx_viz[inod][1];
		xyz_tri[3*k+2] = xx_viz[inod][2];
		nrm_tri[3*k  ] = norm_nod[inod][0];
		nrm_tri[3*k+1] = norm_nod[inod][1];
		nrm_tri[3*k+2] = norm_nod[inod][2];
	};
	return;
};
static void copy_each_triangle_map_postion(long ie_viz[3], double **xx_viz,
											double **d_nod, int icomp,
											double xyz_map[9], double nrm_tri[9], double d_tri[3]){
	double xyz_tri[9];
	long inod;
	int k;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod][icomp];
		xyz_tri[3*k  ] = xx_viz[inod][0];
		xyz_tri[3*k+1] = xx_viz[inod][1];
		xyz_tri[3*k+2] = xx_viz[inod][2];
		nrm_tri[3*k  ] = 0.0;
		nrm_tri[3*k+1] = 0.0;
		nrm_tri[3*k+2] = 1.0;
	};
	projection_patch_4_map(xyz_tri, xyz_map);
	return;
};


int add_each_isoline_npatch(int ist_patch, double v_line, int icomp, struct psf_data *psf_s){
	double d_tri[3], xyz_tri[9];
	int iele, idraw;
	
	int inum_patch = ist_patch;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		copy_each_triangle_postion(&psf_s->ie_viz[iele][0], psf_s->xx_viz, 
								   psf_s->d_nod, icomp, xyz_tri, d_tri);
		/*  find isoline */
		idraw = find_isoline_on_triangle(xyz_tri, d_tri, v_line);
		/*  count isoline */
		inum_patch = inum_patch + 12 * idraw;
	};
	
	return inum_patch;
};

int set_each_isoline_to_buf(int ist_patch, double width, 
                            double v_line, int icomp, double *f_color,
                            struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	double d_tri[3], xyz_tri[9], nrm_tri[9];
	double x_line[6], dir_line[6], norm_line[6], color_line[8];
	int hex_tube[12][3];
	
	int idraw;
	int iele, nd;
	
	long inum_patch = ist_patch;
	copy_hex_tube_pp(hex_tube);
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		copy_each_triangle_postion_norm(&psf_s->ie_viz[iele][0], psf_s->xx_viz, psf_s->norm_nod,
										psf_s->d_nod, icomp, xyz_tri, nrm_tri, d_tri);
		/*  find isoline */
		idraw = set_isoline_on_triangle(x_line, dir_line, norm_line, 
										xyz_tri, nrm_tri, d_tri, v_line);
		/* draw isoline */
		if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line, 
												 x_line, dir_line, norm_line, strided_buf);
		};
	};

	return inum_patch;
};

int set_each_map_isoline_to_buf(int ist_patch, double width, 
							   double v_line, int icomp, double *f_color, 
							   struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	double d_tri[3], nrm_tri[9];
	double xyz_map[9];
	double x_line[6], dir_line[6], norm_line[6], color_line[8];
	int hex_tube[12][3];
	
	int idraw;
	int iele, nd;
	
	int inum_patch = ist_patch;
	copy_hex_tube_pp(hex_tube);
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		copy_each_triangle_map_postion(&psf_s->ie_viz[iele][0], psf_s->xx_viz,
										psf_s->d_nod, icomp, xyz_map, nrm_tri, d_tri);
		idraw = set_isoline_on_triangle(x_line, dir_line, norm_line, 
										xyz_map, nrm_tri, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line,
												 x_line, dir_line, norm_line, strided_buf);
		};
	};
	return inum_patch;
};
