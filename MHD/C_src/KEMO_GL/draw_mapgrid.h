
/* draw_mapgrid.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"

#ifndef DRAW_MAPGRID_
#define DRAW_MAPGRID_

/* prototypes */

void init_mapgrid_position();

void draw_sph_flame_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf);
void draw_map_flame_VBO(const GLdouble *orthogonal, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf);

void draw_flame_4_map(struct buffer_for_gl *gl_buf);
void draw_sph_flame(double radius, struct buffer_for_gl *gl_buf);

#endif
