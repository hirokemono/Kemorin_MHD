
/* set_coastline_to_buf.h */

#ifndef SET_COASTLINE_TO_BUF_
#define SET_COASTLINE_TO_BUF_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "vartex_array_object_gl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "coastline_c.h"

/* prototypes */

void init_mapgrid_position(void);

int count_sph_flame(void);
int set_sph_flame_to_buf(double radius, struct gl_strided_buffer *strided_buf);
int set_map_flame_to_buf(struct gl_strided_buffer *strided_buf);

int count_coastline_buf(void);
int set_coastline_buf(double radius, struct gl_strided_buffer *strided_buf);
int set_map_coastline_buf(struct gl_strided_buffer *strided_buf);
#endif
