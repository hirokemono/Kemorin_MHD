
/* set_mesh_grid_2_gl_buf.h */

#ifndef SET_MESH_GRID_2_GL_BUF_
#define SET_MESH_GRID_2_GL_BUF_

#include <stdlib.h>
#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "rainbow_color_code_c.h"

/* prototypes */

int count_mesh_edge_buf(int *iflag_domain, int *istack_grp, struct viewer_mesh *mesh_s);
int add_mesh_edge_to_buf(int ist_edge, int line_color, int color_mode, int color_loop, GLfloat single_color[4],
			int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain, 
			struct viewer_mesh *mesh_s, struct gl_strided_buffer *mesh_buf);

int count_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
int set_mesh_grid_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct gl_strided_buffer *mesh_buf);

#endif
