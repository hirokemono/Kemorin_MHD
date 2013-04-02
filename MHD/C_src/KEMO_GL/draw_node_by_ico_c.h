
/* draw_node_by_ico_c.h */

#ifndef DRAW_NODE_BY_ICO_C_
#define DRAW_NODE_BY_ICO_C_

#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "rainbow_color_code_c.h"
#include "icosahedron_c.h"
#include "m_surface_mesh_4_viewer_c.h"

/* prototypes */

void draw_node_by_ico(int num_grp, int igrp, int *istack_grp, int *item_grp,
					  struct viewer_mesh *mesh_s, double node_diam,
					  int node_color, int color_mode, int color_loop, GLfloat single_color[4],
					  int *iflag_domain, struct buffer_for_gl *gl_buf);

#endif
