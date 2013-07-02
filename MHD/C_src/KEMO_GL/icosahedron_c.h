
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"

/* prototypes */
void init_icosahedron_c();
int set_icosahedron_patch(double size, double x_draw[3], 
						  double *xyz_draw, double *norm_draw);

int set_tube_vertex(int ncorner, double radius, double x_line[6], double dir_line[6],
					double color_line[8], double *xyz, double *nor, double *col);

void glDrawArrowf(GLfloat x0, GLfloat y0, GLfloat z0,
				  GLfloat x1, GLfloat y1, GLfloat z1,
				  GLfloat ratio);
void glDrawPipef(GLfloat x0, GLfloat y0, GLfloat z0,
				 GLfloat x1, GLfloat y1, GLfloat z1,
				 GLfloat r);

void glDrawPipefv(GLfloat xx[6], GLfloat thickness);

#endif
