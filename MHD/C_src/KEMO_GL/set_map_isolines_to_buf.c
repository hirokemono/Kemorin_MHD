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

static int count_map_isoline(int num_patch, int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int j;
	double v_line;
	
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline, 
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		num_patch = add_each_isoline_npatch(num_patch, v_line, 
											psf_m->icomp_draw_psf, psf_s);
	};
	return num_patch;
}

static int set_map_isolines_to_buf(int ist_patch, int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *psf_buf){
	int inum_patch = ist_patch;
	int j, nd;
	double v_line;
	double f_color[4];
	
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline, 
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		
		if (psf_m->isoline_color == RAINBOW_LINE){	
			set_rainbow_color_code(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf],
								   v_line, f_color);
		};
		inum_patch = set_each_map_isoline_to_buf(inum_patch, psf_m->isoline_width, 
												 v_line, psf_m->icomp_draw_psf,
												 f_color, psf_s, psf_buf);
	};
	
	return inum_patch;
}



int count_map_PSF_isoline(int ist_patch, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			num_patch = count_map_isoline(num_patch, 
										  IZERO, psf_m->ist_positive_line,
										  psf_s, psf_m);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			num_patch = count_map_isoline(num_patch, 
										  psf_m->ist_positive_line, psf_m->n_isoline,
										  psf_s, psf_m);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		num_patch = add_each_isoline_npatch(num_patch, ZERO,
											psf_m->icomp_draw_psf, psf_s);
	};
	return num_patch;
}


int set_map_PSF_isoline_to_buf(int ist_patch,
							   struct psf_data *psf_s, struct psf_menu_val *psf_m,
							   struct gl_strided_buffer *psf_buf){
	double dub_r;
	int inum_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline, 
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			inum_patch = set_map_isolines_to_buf(inum_patch,
						IZERO, psf_m->ist_positive_line,
						psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_patch = set_map_isolines_to_buf(inum_patch,
						psf_m->ist_positive_line, psf_m->n_isoline, 
						psf_s, psf_m, psf_buf);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		dub_r = 2.0 * psf_m->isoline_width;
		inum_patch = set_each_map_isoline_to_buf(inum_patch, dub_r,
												 ZERO, psf_m->icomp_draw_psf, 
												 black, psf_s, psf_buf);
	};
	return inum_patch;
}

