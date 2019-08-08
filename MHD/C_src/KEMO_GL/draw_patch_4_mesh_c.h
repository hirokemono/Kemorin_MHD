
/* draw_patch_4_mesh_c.h */

#ifndef DRAW_PATCH_4_MESH_C_
#define DRAW_PATCH_4_MESH_C_

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "init_gl_lighting_c.h"
#include "rainbow_color_code_c.h"

/* prototypes */
void draw_mesh_patch(int shading_mode, int polygon_mode, int surface_color,
					 int color_mode, int color_loop, double opacity, GLfloat single_color[4], 
					 int num_grp, int *istack_grp, int *item_grp, 
					 double **normal_ele, double **normal_nod, int *isort_grp, 
					 int *ip_domain_far, int igrp, struct viewer_mesh *mesh_s, 
					 int *iflag_domain, struct buffer_for_gl *gl_buf);

#endif
