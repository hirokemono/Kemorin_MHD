
/* draw_coastline.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "coastline_c.h"

#ifndef DRAW_COASTLINE_
#define DRAW_COASTLINE_

/* prototypes */

void draw_coastline_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf);
void draw_map_coastline_VBO(const GLdouble *orthogonal, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf);

void draw_coastline(double radius, struct buffer_for_gl *gl_buf);
void draw_map_coast(struct buffer_for_gl *gl_buf);

#endif
