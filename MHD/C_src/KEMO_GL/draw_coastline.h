
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
void set_sph_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf);
void set_coastline_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *coast_buf);

void set_map_flame_buffer(struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mflame_buf);
void set_map_coastline_buffer(struct mesh_menu_val *mesh_m,
                              struct gl_strided_buffer *coast_buf);


void set_axis_VAO(struct gl_strided_buffer *axis_buf, struct VAO_ids *mesh_VAO);

void map_coastline_grid_VBO(struct gl_strided_buffer *coast_buf,
                            struct gl_strided_buffer *mflame_buf,
                            struct VAO_ids **grid_VAO);

void set_coastline_grid_VBO(struct gl_strided_buffer *coast_buf,
                            struct gl_strided_buffer *grid_buf,
                            struct VAO_ids **grid_VAO);
#endif
