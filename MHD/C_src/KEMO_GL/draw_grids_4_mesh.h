
/* draw_grids_4_mesh.h */

#ifndef DRAW_GRIDS_4_MESH_
#define DRAW_GRIDS_4_MESH_

#include <stdlib.h>
#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "set_mesh_grid_2_gl_buf.h"

/* prototypes */

void draw_mesh_grids_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf);

#endif
