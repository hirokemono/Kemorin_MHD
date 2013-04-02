
/* set_rgb_colors_c.c */

#include "set_rgb_colors_c.h"


double color_normalize_linear_c(double d_min, double d_max, double value){
	double color_norm;
	
	if (d_min >= d_max){
		color_norm = HALF;
	} else if (value < d_min){
		color_norm = ZERO;
	} else if (value >= d_max){
		color_norm = ONE;
	} else {
		color_norm = (value - d_min) / (d_max - d_min);
	};
	
	return color_norm;
}

double color_normalize_linear_segment_c(int n_point, double *value_segment,
			double *segment_color, double value){
	double color_norm;
	int i;
	
	if (value < value_segment[0]){
		color_norm = segment_color[0];
	} else if (value >= value_segment[n_point-1]){
		color_norm = segment_color[n_point-1];
	} else {
		for (i=0;i<n_point-1;i++){
			if(value >= value_segment[i] && value <value_segment[i+1]){
				color_norm = segment_color[i]
						+ (segment_color[i+1] - segment_color[i])
						 * (value - value_segment[i]) / (value_segment[i+1]-value_segment[i]);
				break;
			}
		}
	}
	
	return color_norm;
}


void color_grayscale_c(double rnorm, double *r, double *g, double *b){
	
	if (rnorm < ZERO){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm > ONE){
		*r = ONE;
		*g = ONE;
		*b = ONE;
	} else {
		*r = ONE*rnorm;
		*g = ONE*rnorm;
		*b = ONE*rnorm;
	}
	return;
}


void color_sym_grayscale_c(double rnorm, double *r, double *g, double *b){
	
	if (rnorm < ZERO){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm > ONE){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm >= ZERO && rnorm < HALF){
		*r = ONE*rnorm;
		*g = ONE*rnorm;
		*b = ONE*rnorm;
	} else {
		*r = ONE*(ONE - rnorm);
		*g = ONE*(ONE - rnorm);
		*b = ONE*(ONE - rnorm);
	}
	return;
}

void color_redblue_c(double rnorm, double *r, double *g, double *b){
	double blue =   ZERO;
	double ocean =  ONE / FOUR;
	double green =  ONE / TWO;
	double yellow = THREE / FOUR;
	double red =    ONE;
	
	if (rnorm < blue){
		*r = ZERO;
		*g = ZERO;
		*b = ONE;
	} else if (rnorm >= blue && rnorm < ocean){
		*r = ZERO;
		*g = rnorm * FOUR;
		*b = ONE;
	} else if (rnorm >= ocean && rnorm < green){
		*r = ZERO;
		*g = ONE;
		*b = (green-rnorm) * FOUR;
	} else if (rnorm >= green && rnorm < yellow){
		*r = (rnorm-green) * FOUR;
		*g = ONE;
		*b = ZERO;
	} else if (rnorm >= yellow && rnorm < red){
		*r = ONE;
		*g = (red-rnorm) * FOUR;
		*b = ZERO;
	} else {
		*r = ONE;
		*g = ZERO;
		*b = ZERO;
	}
	return;
}

void color_rainbow_c(double rnorm, double *r, double *g, double *b){
	double purple = ZERO;
	double blue =   DECI;
	double ocean =  0.325;
	double green =  0.55;
	double yellow = 0.775;
	double red =    ONE;
			
	if (rnorm < purple){
		*r = HALF;
		*g = ZERO;
		*b = ONE;
	} else if (rnorm >= purple && rnorm < blue){
		*r = HALF - FIVE*rnorm;
		*g = ZERO;
		*b = ONE;
	} else if (rnorm >= blue && rnorm < ocean){
		*r = ZERO;
		*g = FOURTY*(rnorm-blue) / NINE;
		*b = ONE;
	} else if (rnorm >= ocean && rnorm < green){
		*r = ZERO;
		*g = ONE;
		*b = ONE - FOURTY*(rnorm-ocean) / NINE;
	} else if (rnorm >= green && rnorm < yellow){
		*r = FOURTY*(rnorm-green) / NINE;
		*g = ONE;
		*b = ZERO;
	} else if (rnorm >= yellow && rnorm < red){
		*r = ONE;
		*g = ONE - FOURTY*(rnorm-yellow) / NINE;
		*b = ZERO;
	} else {
		*r = ONE;
		*g = ZERO;
		*b = ZERO;
	}
	return;
}


