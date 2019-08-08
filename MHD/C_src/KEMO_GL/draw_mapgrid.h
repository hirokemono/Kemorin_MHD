
/* draw_mapgrid.h */

#include <math.h>

#include "gl2ps.h"

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "vartex_array_object_gl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"

#ifndef DRAW_MAPGRID_
#define DRAW_MAPGRID_

/* prototypes */

void init_mapgrid_position();
void draw_flame_4_map(struct buffer_for_gl *gl_buf, int iflag_write_ps);
void draw_sph_flame(double radius, struct buffer_for_gl *gl_buf,
                    int iflag_write_ps);

#endif
