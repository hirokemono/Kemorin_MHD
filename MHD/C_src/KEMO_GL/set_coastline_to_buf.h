
/* set_coastline_to_buf.h */

#ifndef SET_COASTLINE_TO_BUF_
#define SET_COASTLINE_TO_BUF_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_vertex_buffer.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "coastline_c.h"

/* prototypes */

void init_mapgrid_position(void);

long count_sph_flame(void);
long set_sph_flame_to_buf(double radius, struct gl_strided_buffer *strided_buf,
                          struct gl_local_buffer_address *point_buf);
long set_map_flame_to_buf(struct gl_strided_buffer *strided_buf,
                          struct gl_local_buffer_address *point_buf);

long count_coastline_buf(void);
long set_coastline_buf(double radius, struct gl_strided_buffer *strided_buf,
                       struct gl_local_buffer_address *point_buf);
long set_map_coastline_buf(struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf);
#endif
