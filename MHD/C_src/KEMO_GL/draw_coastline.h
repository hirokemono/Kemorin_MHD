
/* draw_coastline.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_coastline_to_buf.h"

#ifndef DRAW_COASTLINE_
#define DRAW_COASTLINE_

/* prototypes */

void set_map_flame_VBO(struct VAO_ids *line_VAO, 
			struct gl_strided_buffer *line_buf);
void set_map_coastline_VBO(struct VAO_ids *line_VAO, 
			struct gl_strided_buffer *line_buf);

void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf);

void draw_coastline_VBO(struct mesh_menu_val *mesh_m, struct view_element *view_s, 
			struct VAO_ids **grid_VAO, struct kemoview_shaders *kemo_shaders);

#endif
