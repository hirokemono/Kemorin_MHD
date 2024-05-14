
/* set_mesh_node_2_gl_buf.h */

#ifndef SET_MESH_NODE_2_GL_BUF_
#define SET_MESH_NODE_2_GL_BUF_

#include <math.h>

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "pthread_mesh_patch_to_buf.h"

/* prototypes */
long count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                            long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                            long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch);
long set_mesh_node_to_buf(const int nthread,
                          long *istack_node_domain_patch,  long *istack_node_nod_grp_patch,
                          long *istack_node_ele_grp_patch, long *istack_node_surf_grp_patch,
                          struct viewer_mesh *mesh_s,
                          struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mesh_buf);

#endif
