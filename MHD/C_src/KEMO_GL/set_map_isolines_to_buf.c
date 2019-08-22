/*
 *  set_map_isolines_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_map_isolines_to_buf.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

static int count_map_isoline_to_buf(int ncorner, double v_line, int icomp,
			struct psf_data *psf_s){
	double d_tri[3];
	int inod, iele, k;
	int num_patch = 0;
	
	for(iele = 0; iele < psf_s->nele_viz; iele++) {
		for(k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] = psf_s->d_nod[inod][icomp];
		};
		
		/*  draw isoline */
		if(mark_isoline_on_patch_c(d_tri, v_line) == 1){num_patch = num_patch + 2*ncorner;};
	};
	return num_patch;
};

static int set_map_isoline_to_buf(int ist_buf, int ncorner, double width, 
			double v_line, int icomp, double *f_color, 
			struct psf_data *psf_s, struct gl_strided_buffer *strided_buf){
	int icou_buf;
	double d_tri[3], xx_tri[9];
	double xyz_map[9];
	double x_line[6], dir_line[6], color_line[8];
	
	int idraw;
	int inod, iele, k, nd;
	
	icou_buf = ist_buf;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoline_on_patch_c(x_line, dir_line, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			icou_buf = set_tube_strided_buffer(icou_buf, ncorner, width, 
						x_line, dir_line, color_line, strided_buf);
		};
	};
	return icou_buf;
};


static int count_map_zeroline(int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_patch = count_map_isoline_to_buf(ncorner, ZERO, psf_m->icomp_draw_psf, psf_s);
	return num_patch;
}

static int count_map_isoline(int ist, int ied, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int j;
	double v_line;
	
	int num_patch = 0;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		num_patch = num_patch 
				+ count_map_isoline_to_buf(ncorner, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	return num_patch;
}


static int set_map_zeroline_to_buf(int ist_buf, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	int inum_buf = ist_buf;
	inum_buf = set_map_isoline_to_buf(inum_buf, ncorner, 0.05, 
				ZERO, psf_m->icomp_draw_psf, black, psf_s, psf_buf);
	return inum_buf;
}

static int set_map_isolines_to_buf(int ist_buf, int ist, int ied, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *psf_buf){
	int inum_buf = ist_buf;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		
		if (psf_m->isoline_color == RAINBOW_LINE){	
			set_rainbow_color_code(psf_m->cmap_psf, v_line, f_color);
		};
		inum_buf = set_map_isoline_to_buf(inum_buf, ncorner, 0.02, 
					v_line, psf_m->icomp_draw_psf, f_color, psf_s, psf_buf);
	};
	
	return inum_buf;
}



int count_map_PSF_isoline(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_patch = 0;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			num_patch = num_patch + count_map_isoline(IZERO, psf_m->ist_positive_line,
						ncorner, psf_s, psf_m);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			num_patch = num_patch + count_map_isoline(psf_m->ist_positive_line, psf_m->n_isoline, 
						ncorner, psf_s, psf_m);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		num_patch = num_patch + count_map_zeroline(ncorner, psf_s, psf_m);
	};
	return num_patch;
}


int set_map_PSF_isoline_to_buf(int ist_buf, int ncorner, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	int inum_buf = ist_buf;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			inum_buf = set_map_isolines_to_buf(inum_buf,
						IZERO, psf_m->ist_positive_line,
						ncorner, psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_buf = set_map_isolines_to_buf(inum_buf,
						psf_m->ist_positive_line, psf_m->n_isoline, 
						ncorner, psf_s, psf_m, psf_buf);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		inum_buf = set_map_zeroline_to_buf(inum_buf, ncorner, psf_s, psf_m, psf_buf);
	};
	return inum_buf;
}

