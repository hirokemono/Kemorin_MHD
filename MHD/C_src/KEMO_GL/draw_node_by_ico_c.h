
/* draw_node_by_ico_c.h */

#ifndef DRAW_NODE_BY_ICO_C_
#define DRAW_NODE_BY_ICO_C_

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "icosahedron_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "set_mesh_node_2_gl_buf.h"

/* prototypes */

void draw_mesh_nodes_ico_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf);
#endif
