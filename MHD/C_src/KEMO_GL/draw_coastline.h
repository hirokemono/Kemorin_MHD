
/* draw_coastline.h */

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_coastline_to_buf.h"
#include "set_axis_to_buf.h"

#ifndef DRAW_COASTLINE_
#define DRAW_COASTLINE_

/* prototypes */

void set_map_flame_buffer(int iflag_draw_sph_grid,
                          struct gl_strided_buffer *mflame_buf);
void set_map_coastline_buffer(int iflag_draw_coast,
                              struct gl_strided_buffer *coast_buf);


void set_axis_VAO(struct mesh_menu_val *mesh_m, struct view_element *view_s,
			struct VAO_ids *mesh_VAO);

void map_coastline_grid_VBO(struct mesh_menu_val *mesh_m, struct VAO_ids **grid_VAO);

void set_coastline_grid_VBO(struct mesh_menu_val *mesh_m, struct VAO_ids **grid_VAO);
#endif
