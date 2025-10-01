/*
// set_rgb_colors_c.c 
*/

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
	double color_norm = 0.0;
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
