
/* draw_coastline.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "vartex_array_object_gl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "coastline_c.h"

#ifndef DRAW_COASTLINE_
#define DRAW_COASTLINE_

/* prototypes */

void draw_coastline(double radius, struct buffer_for_gl *gl_buf);
void draw_map_coast(struct buffer_for_gl *gl_buf);

#endif
