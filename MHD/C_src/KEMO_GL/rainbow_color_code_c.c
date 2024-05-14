
/* rainbow_color_code_c.c */

#include "rainbow_color_code_c.h"

static const float black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static const float white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};
static const float l_green[4] = {L_GREEN_R,L_GREEN_G,L_GREEN_B,L_GREEN_A};
/* static const float d_green[4] = {D_GREEN_R,D_GREEN_G,D_GREEN_B,D_GREEN_A}; */

static double c_code[4];

static int set_cyclic_id_by_N_c(int inum, int iloop, int i_min, int i_max){
	int ival;
	int irest, i_int, ist, i, ncomp;
	
    ncomp = i_max-i_min+1;
	irest = ncomp % iloop;
	i_int = 1 + (ncomp-irest) / iloop;
	ist = i_min;
	ival = i_min;
	if (inum > i_min){
		for (i = i_min; i < inum; i++){
			ival = ival + i_int;
			if(ival > i_max){
				ival = ist + 1;
				ist = ival;
			}
		};
	};
    
	return ival;
}

int set_same_id_by_N_c(int inum, int iloop, int i_min, int i_max){
	int ival;
	int irest, i_int, ncomp;
	
    ncomp = i_max-i_min+1;
	irest = ncomp % iloop;
	i_int = (ncomp-irest) / iloop;
	ival = (inum%iloop)*i_int;
	return ival;
}

/*
static void convert_anaglyph_color(double ratio_mono, double *f_color){
	float gray;
	gray = (ONE - ratio_mono) * (f_color[0] + f_color[1] + f_color[2])
		* 0.3333333333333333333333;
	f_color[0] = f_color[0] * ratio_mono + gray;
	f_color[1] = f_color[1] * ratio_mono + gray;
	f_color[2] = f_color[2] * ratio_mono + gray;
	return ;
};
*/

static void get_rainbow_c(double val, double  minval,double  maxval, double *f_color) {
	double f = color_normalize_linear_c(minval, maxval, val);
	colormap_rainbow_c(f, &f_color[0], &f_color[1], &f_color[2]);
	return;
}


static void get_dark_rainbow_c(double val, double minval, double maxval, double *f_color){
	get_rainbow_c(val, minval, maxval, f_color);
	
	f_color[0] = f_color[0]*0.6;
	f_color[1] = f_color[1]*0.6;
	f_color[2] = f_color[2]*0.6;
	return;
}

static void get_blight_rainbow_c(double val, double minval, double maxval, double *f_color){
	get_rainbow_c(val, minval, maxval, f_color);
	
	f_color[0] = f_color[0]*0.6 + FOUR_DECI;
	f_color[1] = f_color[1]*0.6 + FOUR_DECI;
	f_color[2] = f_color[2]*0.6 + FOUR_DECI;
	return ;
};

static void get_grayscale_c(double val, double minval, double maxval, 
			double mincolor, double maxcolor, double *f_color){
	double f, r, g, b;
	
	f = color_normalize_linear_c(minval, maxval, val);
	colormap_grayscale_c(f, &r, &g, &b);
	
	f_color[0] = (maxcolor-mincolor) * r;
	f_color[1] = (maxcolor-mincolor) * g;
	f_color[2] = (maxcolor-mincolor) * b;
	return;
}

void set_two_color_scale_c(double val, double *f_color){
	
	if ( val < 0.0 ) {
		f_color[0] = 0.2;
		f_color[1] = 0.5;
		f_color[2] = 0.8;
	}
	else{
		f_color[0] = 1.0;
		f_color[1] = 0.5;
		f_color[2] = 0.0;
	};
	
	/*
	f_color[3] = opacity * SIX_DECI;
	f_color[3] = opacity;
	*/
	return;
}

void set_two_color_scale_g(double val, double *f_color){
	
	if ( val < 0.0 ) {
		f_color[0] = 0.25;
		f_color[1] = 0.25;
		f_color[2] = 0.25;
	}
	else{
		f_color[0] = 0.75;
		f_color[1] = 0.75;
		f_color[2] = 0.75;
	};
	
	/*
	f_color[3] = opacity * SIX_DECI;
	f_color[3] = opacity;
	*/
	return;
}

void set_rainbow_color_code(struct colormap_array *cmap_array,
                            struct colormap_array *omap_array,
                            int id_color_mode, double val_pe,
							double *f_color){
	set_rgb_from_value_s(cmap_array, id_color_mode, val_pe,
                         &f_color[0], &f_color[1], &f_color[2]);

    f_color[3] = set_opacity_from_value_s(omap_array, val_pe);
	return;
}

static void get_grayscale_int_c(int inum, int iloop, int i_min, int i_max, double *f_color){
	int ival;
	double val_pe, val_min, val_max;
	
	ival = set_cyclic_id_by_N_c(inum, iloop, i_min, i_max);
	
	val_min = i_min;
	val_max = i_max;
	val_pe =  ival;
	get_grayscale_c(val_pe, val_min, val_max, ZERO, ONE, f_color);
	return;
}

static void get_b_grayscale_int_c(int inum, int iloop,
			int i_min, int i_max, double *f_color){
	int ival;
	double val_pe, val_min, val_max;
	
	ival = set_cyclic_id_by_N_c(inum, iloop, i_min, i_max);
	
	val_min = i_min;
	val_max = i_max;
	val_pe =  ival;
	get_grayscale_c(val_pe, val_min, val_max, TWO_DECI, NINE_DECI, f_color);
	return;
}

static void get_b_rainbow_int_c(int inum, int iloop, int i_min, int i_max, double *f_color){;
	int ival;
	double val_pe, val_min, val_max;
	
	ival = set_cyclic_id_by_N_c(inum, iloop, i_min, i_max);
	
	val_min = (double) i_min;
	val_max = (double) i_max;
	val_pe =  (double) ival;
	get_blight_rainbow_c(val_pe, val_min, val_max, f_color);
	return;	
}


static void get_d_rainbow_int_c(int inum, int iloop, int i_min, int i_max, double *f_color){
	int ival;
	double val_pe, val_min, val_max;
	
	ival = set_cyclic_id_by_N_c(inum, iloop, i_min, i_max);
	
	val_min = i_min;
	val_max = i_max;
	val_pe =  ival;
	get_dark_rainbow_c(val_pe, val_min, val_max, f_color);
	return;
}

void set_patch_color_mode_c(int surface_color, int color_mode, int color_loop, 
							int ip, int num_pe, int igrp, int num_grp, 
							double opacity, float single_color[4], double *f_color){
	int i;
	if (surface_color == GROUP_COLOR) {
		if(color_mode == GRAYSCALE){
			get_b_grayscale_int_c( (igrp+1), color_loop, IONE, num_grp, &f_color[0]);
		} else {
			get_b_rainbow_int_c( (igrp+1), color_loop, IONE, num_grp, &f_color[0]);
		};
	}
	else if (surface_color == DOMAIN_COLOR) {
		if(color_mode == GRAYSCALE){
			get_b_grayscale_int_c( (ip+1), color_loop, IONE, num_pe, &f_color[0]);
		} else {
			get_b_rainbow_int_c( (ip+1), color_loop, IONE, num_pe, &f_color[0]);
		};
	}
	else if (surface_color == SINGLE_COLOR) {
		for (i=0; i<3; i++) {f_color[i] = single_color[i];};
	}
	else if (surface_color == GREEN_SURFACE) {
		for (i=0; i<3; i++) {f_color[i] = l_green[i];};
	}
	else if (surface_color == WHITE_SURFACE) {
		for (i=0; i<3; i++) {f_color[i] = white[i];};
	};
	
	f_color[3] = opacity;
	return;
};

void set_grid_color_mode_c(int line_color, int color_mode, int color_loop, 
						   int ip, int num_pe, int igrp, int num_grp,
                           float single_color[4], double *f_color){
	int i;
	if (line_color == DOMAIN_COLOR) {
		if(color_mode == GRAYSCALE){
			get_grayscale_int_c( (ip+1), color_loop, IONE, num_pe, f_color);
		} else {
			get_d_rainbow_int_c( (ip+1), color_loop, IONE, num_pe, f_color);
		}
	}
	else if (line_color == GROUP_COLOR) {
		if(color_mode == GRAYSCALE){
			get_grayscale_int_c( (igrp+1), color_loop, IONE, num_grp, f_color);
		} else {
			get_d_rainbow_int_c( (igrp+1), color_loop, IONE, num_grp, f_color);
		};
	}
	else if (line_color == SINGLE_COLOR) {
		for (i=0; i<3; i++) {f_color[i] = single_color[i];};
	}
	else if (line_color == GREEN_SURFACE) {
		for (i=0; i<3; i++) {f_color[i] = l_green[i];};
	}
	else if (line_color == BLACK_LINE) {
		for (i=0; i<3; i++) {f_color[i] = black[i];};
	};
	f_color[3] = ONE;
	return;
};

void set_node_color_mode_c(int surface_color, int color_mode, int color_loop, 
			int igrp, int num_grp, float single_color[4]){
	
	if (surface_color == GROUP_COLOR) {
		igrp = igrp+1;
		if(color_mode == GRAYSCALE){
			get_grayscale_int_c(igrp, color_loop, IZERO, num_grp, c_code);
			c_code[3] = ONE;
		} else {
			get_d_rainbow_int_c(igrp, color_loop, IZERO, num_grp, c_code);
			c_code[3] = ONE;
		}
	}
	 return;
};

void set_black_color_c(double *f_color){
	int i;
	for (i=0; i<3; i++) {f_color[i] = black[i];};
    f_color[3] = 1.0;
	return;
};

void copy_rgba_color_c(float input_color4[4], float copy_color4[4]){
	int i;
	for(i=0;i<4;i++) copy_color4[i] = input_color4[i];
	return;
}
