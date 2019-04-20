
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"

/* prototypes */
void init_icosahedron_c();
int set_icosahedron_patch(double size, double x_draw[3], 
						  double *xyz_draw, double *norm_draw);

int set_tube_vertex(int ncorner, float radius, float x_line[6], float dir_line[6],
					float color_line[8], float *xyz, float *nor, float *col);
int set_cone_vertex(int ncorner, float radius, float x_line[6], float dir_line[6],
                    float color_line[8], float *xyz, float *nor, float *col);
#endif
