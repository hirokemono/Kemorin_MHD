
/* draw_grids_4_mesh.h */

#ifndef DRAW_GRIDS_4_MESH_
#define DRAW_GRIDS_4_MESH_

#include <stdlib.h>
#include "kemoviewer_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "rainbow_color_code_c.h"

/* prototypes */

void draw_mesh_grid(int line_color, int color_mode, int color_loop, GLfloat single_color[4],
					int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain, 
					struct viewer_mesh *mesh_s, struct buffer_for_gl *gl_buf);
	
#endif
