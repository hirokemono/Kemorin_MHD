
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"

/* prototypes */
long num_icosahedron_patch(void);


void init_icosahedron_c(void);

long set_icosahedron_node_index(double size, double x_draw[3],
                                double xyzw_draw[48], double norm_draw[48],
                                unsigned int ie_ico[60]);
long set_icosahedron_patch(double size, double x_draw[3],
                           double *xyzw_draw, double *norm_draw);


void find_normal_on_line(double norm_line[4],
                         const double dir_line[4]);
int set_tube_vertex(int ncorner, double radius, 
                    double xyzw_line[8], double dir_line[8], 
                    double norm_line[8], double color_line[8], 
                    double *xyzw, double *norm, double *col);


int set_cone_node_index(int ncorner, double radius,
                        double xyzw_line[8], double dir_line[8],
                        double norm_line[8], double color_line[8],
                        double *xyzw, double *norm, double *col,
                        unsigned int *ie_cone);
int set_cone_vertex(int ncorner, double radius,
                    double xyzw_line[8], double dir_line[8],
                    double norm_line[8], double color_line[8], 
                    double *xyzw, double *norm, double *col);


#endif
