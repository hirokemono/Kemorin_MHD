
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"

/* prototypes */
void init_icosahedron_c(void);
long set_icosahedron_patch(double size, double x_draw[3], 
                           double *xyzw_draw, double *norm_draw);

int set_cone_vertex(int ncorner, double radius, double x_line[6], double dir_line[8],
                    double color_line[8], double *xyzw, double *norm, double *col);


long set_tube_strided_buffer(const long ist_tube, int ncorner, double radius,
                             double x_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf);
#endif
