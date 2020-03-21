/*
 *  cal_surface_center_normal_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/23.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef CAL_SURFACE_CENTER_NORMAL_C_
#define CAL_SURFACE_CENTER_NORMAL_C_

#include <math.h>
#include "calypso_param_c.h"

/* prototypes */

void cal_center_4_quad_c(const double x1[3], const double x2[3], const double x3[3], const double x4[3], 
						 double center[3]);
void cal_center_4_triangle_c(const double x1[3], const double x2[3], const double x3[3], 
							 double center[3]);

double cal_normal_4_quad_c(const double x1[3], const double x2[3], const double x3[3], const double x4[3], 
						   double normal[3]);
double cal_normal_4_triangle_c(const double x1[3], const double x2[3], const double x3[3], 
							   double normal[3]);

#endif
