
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "vartex_array_object_gl.h"

/* prototypes */
void init_icosahedron_c();
int set_icosahedron_patch(double size, double x_draw[3], 
						  double *xyz_draw, double *norm_draw);

int set_tube_vertex(int ncorner, double radius, double x_line[6], double dir_line[6],
					double color_line[8], double *xyz, double *nor, double *col);
int set_cone_vertex(int ncorner, double radius, double x_line[6], double dir_line[6],
                    double color_line[8], double *xyz, double *nor, double *col);


int set_tube_strided_buffer(int ist_buf, int ncorner, double radius, 
			double x_line[6], double dir_line[6], double color_line[8], 
			struct gl_strided_buffer *strided_buf);
#endif
