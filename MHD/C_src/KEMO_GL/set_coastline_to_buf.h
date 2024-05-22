
/* set_coastline_to_buf.h */

#ifndef SET_COASTLINE_TO_BUF_
#define SET_COASTLINE_TO_BUF_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "coastline_c.h"
#include "set_primitives_to_gl_buffer.h"

/* prototypes */

void init_mapgrid_position(void);

long count_sph_frame_line(void);
long count_sph_med_flame(void);
long count_sph_long_flame(void);

long set_sph_med_flame_line_to_buf(long ist_buf,  long ist_edge, long ied_edge,
                                   long num_grid, double radius,
                                   struct gl_strided_buffer *strided_buf);
long set_sph_long_flame_line_to_buf(long ist_buf,  long ist_edge, long ied_edge,
                                    long num_grid, double radius,
                                    struct gl_strided_buffer *strided_buf);
long set_sph_med_flame_tube_to_buf(long ist_buf, long ist_edge, long ied_edge, long num_grid,
                                   int ncorner, double tube_radius, double radius,
                                   struct gl_strided_buffer *strided_buf);
long set_sph_long_flame_tube_to_buf(long ist_buf, long ist_edge, long ied_edge, long num_grid,
                                    int ncorner, double tube_radius, double radius,
                                    struct gl_strided_buffer *strided_buf);

long set_map_med_frame_line_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                   long num_grid, struct gl_strided_buffer *strided_buf);
long set_long_map_flame_line_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                    long num_grid, struct gl_strided_buffer *strided_buf);
long set_map_med_frame_tube_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                   long num_grid, int ncorner, double tube_radius,
                                   struct gl_strided_buffer *strided_buf);
long set_map_long_frame_tube_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                    long num_grid, int ncorner, double tube_radius,
                                    struct gl_strided_buffer *strided_buf);

long set_coastline_line_buf(long ist_buf, long ist_edge, long ied_edge,
                            double radius, struct gl_strided_buffer *strided_buf);
long set_coastline_tube_buf(long ist_buf, long ist_edge, long ied_edge,
                            int ncorner, double tube_radius, double radius,
                            struct gl_strided_buffer *strided_buf);

long set_map_coastline_line_buf(long ist_buf, long ist_edge, long ied_edge,
                                struct gl_strided_buffer *strided_buf);
long set_map_coastline_tube_buf(long ist_buf, long ist_edge, long ied_edge,
                                int ncorner, double tube_radius,
                                struct gl_strided_buffer *strided_buf);

long set_tangent_cylinder_line_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                      long num_grid, double radius, double r_ICB,
                                      struct gl_strided_buffer *strided_buf);
long set_tangent_cylinder_tube_to_buf(long ist_buf, long ist_edge, long ied_edge,
                                      long num_grid, int ncorner, double tube_radius,
                                      double radius, double r_ICB,
                                      struct gl_strided_buffer *strided_buf);

#endif
