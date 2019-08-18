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

static int count_map_isoline_to_buf(double width, double v_line, int icomp,
									 struct psf_data *psf_s){
	int num_quad;
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	double xyz_map[9];
	
	int idraw;
	int inod, iele, k;
	
	num_quad = 0;
	for(iele = 0; iele < psf_s->nele_viz; iele++) {
		for(k = 0; k < 3; k++) {
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){num_quad = num_quad + 1;};
	};
	return num_quad;
};

static int set_map_isoline_to_buf(int ist_line, double width, double v_line, int icomp,
									 double *f_color, struct psf_data *psf_s,
									 struct gl_strided_buffer *strided_buf){
	int inum_line;
	double d_tri[3], xx_tri[9];
	double x_ribbon[18];
	double xyz_map[9];
	
	int idraw;
	int inod, iele, k, nd, k1;
	
	inum_line = ist_line;
	for (iele = 0; iele < psf_s->nele_viz; iele++) {
		for (k = 0; k < 3; k++) {
			
			inod = psf_s->ie_viz[iele][k] - 1;
			d_tri[k] =      psf_s->d_nod[inod][icomp];
			xx_tri[3*k  ] = psf_s->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s->xx_viz[inod][2];
		};
		
		projection_patch_4_map(xx_tri, xyz_map);
		idraw = find_isoribbon_on_patch_c(x_ribbon, width, xyz_map, d_tri, v_line);
		/*  draw isoline */
		if(idraw == 1){
			for(k1=0;k1<6;k1++){
				set_node_stride_VBO((6*inum_line+k1), strided_buf);
				strided_buf->x_draw[0] =  x_ribbon[3*k1  ];
				strided_buf->x_draw[1] =  x_ribbon[3*k1+1];
				strided_buf->x_draw[2] =  0.01;
				for(nd=0;nd<4;nd++) strided_buf->c_draw[nd] = f_color[nd];
			};
			inum_line = inum_line + 1;
		};
	};
	return inum_line;
};


static int count_map_zeroline(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_line = count_map_isoline_to_buf(0.005, ZERO, psf_m->icomp_draw_psf, psf_s);
	return num_line;
}

static int count_map_isoline(int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int j;
	double v_line;
	
	int num_line = 0;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m);
		num_line = num_line 
				+ count_map_isoline_to_buf(0.002, v_line, psf_m->icomp_draw_psf, psf_s);
	};
	return num_line;
}


static int set_map_zeroline_to_buf(int ist_line, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	int inum_line = ist_line;
	inum_line = set_map_isoline_to_buf(inum_line, 0.005, ZERO, psf_m->icomp_draw_psf, black,
							  psf_s, psf_buf);
	return inum_line;
}

static int set_map_isolines_to_buf(int ist_line, int ist, int ied, 
			struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *psf_buf){
	int inum_line = ist_line;
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
		inum_line = set_map_isoline_to_buf(inum_line, 0.002, v_line, psf_m->icomp_draw_psf, f_color, 
								  psf_s, psf_buf);
	};
	
	return inum_line;
}



int count_map_PSF_isoline(struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int num_line = 0;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			num_line = num_line + count_map_isoline(IZERO, psf_m->ist_positive_line,
						psf_s, psf_m);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			num_line = num_line + count_map_isoline(psf_m->ist_positive_line, psf_m->n_isoline, 
						psf_s, psf_m);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		num_line = num_line + count_map_zeroline(psf_s, psf_m);
	};
	return num_line;
}


int set_map_PSF_isoline_to_buf(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct gl_strided_buffer *psf_buf){
	int inum_line = 0;
	if(psf_m->draw_psf_grid  != 0){
		find_start_positive_lines(psf_m);
		if(psf_m->ist_positive_line > 1){
			inum_line = set_map_isolines_to_buf(inum_line,
						IZERO, psf_m->ist_positive_line,
						psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_line = set_map_isolines_to_buf(inum_line,
						psf_m->ist_positive_line, psf_m->n_isoline, 
						psf_s, psf_m, psf_buf);
		};
	};
	if(psf_m->draw_psf_zero  != 0){
		inum_line = set_map_zeroline_to_buf(inum_line, psf_s, psf_m, psf_buf);
	};
	return inum_line;
}

