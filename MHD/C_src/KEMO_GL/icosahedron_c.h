
/* icosahedron_c.h */

#ifndef ICOSAHEDRON_C_
#define ICOSAHEDRON_C_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"

/* prototypes */
void init_icosahedron_c(void);

int num_icosahedron_node(void);
int num_icosahedron_patch(void);
long set_icosahedron_node_index(double size, double x_draw[3],
                                double xyzw_draw[48], double norm_draw[48],
                                unsigned int ie_ico[60]);

void find_normal_on_line(double norm_line[4],
                         const double dir_line[4]);

int num_tube_node(int ncorner);
int num_tube_patch(int ncorner);
int set_tube_node_index(int ncorner, double radius,
                        double xyzw_line[8], double dir_line[8],
                        double norm_line[8], double color_line[8],
                        double *xyzw, double *norm, double *col,
                        unsigned int *ie_tube);

int num_cone_node(int ncorner);
int num_cone_patch(int ncorner);
int set_cone_node_index(int ncorner, double radius,
                        double xyzw_line[8], double dir_line[8],
                        double norm_line[8], double color_line[8],
                        double *xyzw, double *norm, double *col,
                        unsigned int *ie_cone);

int num_arrow_node(int ncorner);
int num_arrow_patch(int ncorner);
int set_arrow_node_index(int ncorner, double radius,
                         double xyzw_line[8], double dir_line[8],
                         double norm_line[8], double color_line[8],
                         double *xyzw, double *norm, double *col,
                         unsigned int *ie_arrow);
#endif
