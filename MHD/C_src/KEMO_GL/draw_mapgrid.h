
/* draw_mapgrid.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"

#ifndef DRAW_MAPGRID_
#define DRAW_MAPGRID_

/* prototypes */

void init_mapgrid_position();
void draw_flame_4_map(struct buffer_for_gl *gl_buf);
void draw_sph_flame(double radius, struct buffer_for_gl *gl_buf);

#endif
