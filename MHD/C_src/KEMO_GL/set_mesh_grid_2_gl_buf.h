
/* set_mesh_grid_2_gl_buf.h */

#ifndef SET_MESH_GRID_2_GL_BUF_
#define SET_MESH_GRID_2_GL_BUF_

#include <stdlib.h>
#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "pthread_mesh_patch_to_buf.h"

/* prototypes */

long count_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                            long *istack_edge_domain_edge,
                            long *istack_ele_grp_edge,
                            long *istack_surf_grp_edge);
long set_mesh_grid_to_buf(int nthread, long *istack_edge_domain_edge,
                          long *istack_ele_grp_edge, long *istack_surf_grp_edge,
                          struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                          struct gl_strided_buffer *mesh_buf);

#endif
