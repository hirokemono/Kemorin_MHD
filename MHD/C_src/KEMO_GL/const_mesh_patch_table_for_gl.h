/*
 *  const_mesh_patch_table_for_gl.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef CONST_MESH_PATCH_TABLE_FOR_GL_
#define CONST_MESH_PATCH_TABLE_FOR_GL_

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "rainbow_color_code_c.h"


long count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
long set_solid_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                   struct viewer_mesh *mesh_s,
                                   long *iele_solid_patch);

long count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
long set_transparent_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                         struct viewer_mesh *mesh_s, 
                                         long *iele_trans_patch);

void set_mesh_patch_colors(struct mesh_menu_val *mesh_m, struct viewer_mesh *mesh_s);
void set_trans_mesh_patch_for_sort(struct viewer_mesh *mesh_s,
                                   const long *iele_trans_patch, const double *z_ele_view,
                                   float *z_trans_patch, long *index_trans_patch);

#endif /* CONST_MESH_PATCH_TABLE_FOR_GL_ */

