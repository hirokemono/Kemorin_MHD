
/* set_mesh_patch_2_gl_buf.h */

#ifndef SET_MESH_PATCH_2_GL_BUF_
#define SET_MESH_PATCH_2_GL_BUF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "rainbow_color_code_c.h"

/* prototypes */

int count_mesh_patch_buf(int *istack_grp, int *ip_domain_far,
			struct viewer_mesh *mesh_s, int *iflag_domain);
int add_mesh_patch_to_buf(int ist_tri, int shading_mode, int polygon_mode, int surface_color,
			int color_mode, int color_loop, double opacity, GLfloat single_color[4], 
			int num_grp, int *istack_grp, int *item_grp, 
			double **normal_ele, double **normal_nod, int *isort_grp, 
			int *ip_domain_far, int igrp, struct viewer_mesh *mesh_s, int *iflag_domain, 
			struct gl_strided_buffer *mesh_buf);


int count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
int count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);

int set_solid_mesh_patches_to_buf(int shading_mode, struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m, struct gl_strided_buffer *mesh_buf);
int set_transparent_mesh_patches_to_buf(int shading_mode, struct viewer_mesh *mesh_s, 
			struct mesh_menu_val *mesh_m, struct gl_strided_buffer *mesh_buf);

#endif
