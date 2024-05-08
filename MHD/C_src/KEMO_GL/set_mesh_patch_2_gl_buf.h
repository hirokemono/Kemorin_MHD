
/* set_mesh_patch_2_gl_buf.h */

#ifndef SET_MESH_PATCH_2_GL_BUF_
#define SET_MESH_PATCH_2_GL_BUF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"

/* prototypes */

long count_solid_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);
long count_transparent_mesh_patches(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m);

void add_mesh_patch_to_buffer(int shading_mode, int polygon_mode,
                              struct viewer_mesh *mesh_s,
                              long ntot_patch, long *iele_patch,
                              struct gl_strided_buffer *mesh_buf);

long set_solid_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                   struct viewer_mesh *mesh_s,
                                   long *iele_solid_patch);
long set_transparent_mesh_patches_to_buf(struct mesh_menu_val *mesh_m,
                                         struct viewer_mesh *mesh_s,
                                         long *iele_trans_patch);

void set_trans_mesh_patch_for_sort(struct viewer_mesh *mesh_s,
                                   const long *iele_trans_patch, const double *z_ele_view,
                                   float *z_trans_patch, long *index_trans_patch);

void set_mesh_patch_colors(struct mesh_menu_val *mesh_m, struct viewer_mesh *mesh_s);
#endif
