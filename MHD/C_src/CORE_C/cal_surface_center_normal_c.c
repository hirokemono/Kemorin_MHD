/*
 *  cal_surface_center_normal_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/23.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "cal_surface_center_normal_c.h"

void cal_center_4_quad_c(const double x1[3], const double x2[3], const double x3[3], const double x4[3], 
						 double center[3]){
	double quad;
	int nd;
	
	quad = ONE / FOUR;
	for (nd = 0; nd < 3; nd++){center[nd] = (x1[nd] + x2[nd] + x3[nd] + x4[nd]) * quad;};
	
	return;
};

void cal_center_4_triangle_c(const double x1[3], const double x2[3], const double x3[3], 
							 double center[3]){
	double third;
	int nd;
	
	third = ONE / THREE;
	for (nd = 0; nd < 3; nd++){center[nd] = (x1[nd] + x2[nd] + x3[nd]) * third;};
	
	return;
};

double cal_normal_4_quad_c(const double x1[3], const double x2[3], const double x3[3], const double x4[3], 
						   double normal[3]){
	double size;
	
	normal[0] = (x3[1] - x1[1]) * (x4[2] - x2[2]) 
			  - (x3[2] - x1[2]) * (x4[1] - x2[1]);
	normal[1] = (x3[2] - x1[2]) * (x4[0] - x2[0])
			  - (x3[0] - x1[0]) * (x4[2] - x2[2]);
	normal[2] = (x3[0] - x1[0]) * (x4[1] - x2[1])    
			  - (x3[1] - x1[1]) * (x4[0] - x2[0]);
	
	size = sqrt(normal[0]*normal[0] + normal[1]*normal[1] + normal[2]*normal[2]);
	if(size >= EPSILON){
		normal[0] = normal[0] / size;
		normal[1] = normal[1] / size;
		normal[2] = normal[2] / size;
	} else {
		normal[0] = ZERO;
		normal[1] = ZERO;
		normal[2] = ZERO;
    };
	size = size * HALF;
	
	return size;
};


double cal_normal_4_triangle_c(const double x1[3], const double x2[3], const double x3[3], 
							   double normal[3]){
	double size;
	
	normal[0] = (x2[1] - x1[1]) * (x3[2] - x1[2]) 
			  - (x2[2] - x1[2]) * (x3[1] - x1[1]);
	normal[1] = (x2[2] - x1[2]) * (x3[0] - x1[0])
			  - (x2[0] - x1[0]) * (x3[2] - x1[2]);
	normal[2] = (x2[0] - x1[0]) * (x3[1] - x1[1])    
			  - (x2[1] - x1[1]) * (x3[0] - x1[0]);
	
	size = sqrt(normal[0]*normal[0] + normal[1]*normal[1] + normal[2]*normal[2]);
	if(size >= EPSILON){
		normal[0] = normal[0] / size;
		normal[1] = normal[1] / size;
		normal[2] = normal[2] / size;
	} else {
		normal[0] = ZERO;
		normal[1] = ZERO;
		normal[2] = ZERO;
	};
	size = size * HALF;
	
	return size;
};
