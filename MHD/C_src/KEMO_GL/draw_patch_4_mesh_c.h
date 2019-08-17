
/* draw_patch_4_mesh_c.h */

#ifndef DRAW_PATCH_4_MESH_C_
#define DRAW_PATCH_4_MESH_C_

#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_mesh_patch_2_gl_buf.h"
#include "sort_by_patch_distance.h"

/* prototypes */
void draw_solid_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf);
void draw_trans_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf);
#endif
