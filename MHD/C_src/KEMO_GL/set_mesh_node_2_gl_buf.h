
/* set_mesh_node_2_gl_buf.h */

#ifndef SET_MESH_NODE_2_GL_BUF_
#define SET_MESH_NODE_2_GL_BUF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "icosahedron_c.h"
#include "rainbow_color_code_c.h"

/* prototypes */
int count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
int set_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct gl_strided_buffer *mesh_buf);

#endif
