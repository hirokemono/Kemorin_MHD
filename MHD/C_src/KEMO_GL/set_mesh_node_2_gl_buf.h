
/* set_mesh_node_2_gl_buf.h */

#ifndef DRAW_NODE_BY_ICO_C_
#define DRAW_NODE_BY_ICO_C_

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "icosahedron_c.h"
#include "rainbow_color_code_c.h"

/* prototypes */
int count_mesh_node_ico_to_buf(int *istack_grp, struct viewer_mesh *mesh_s, int *iflag_domain);
int set_mesh_node_ico_to_buf(int ist_tri, int num_grp, int igrp, int *istack_grp, int *item_grp,
			struct viewer_mesh *mesh_s, double node_diam,
			int node_color, int color_mode, int color_loop, GLfloat single_color[4], 
			int *iflag_domain, struct gl_strided_buffer *mesh_buf);


int count_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
int set_mesh_node_to_buf(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct gl_strided_buffer *mesh_buf);

#endif
