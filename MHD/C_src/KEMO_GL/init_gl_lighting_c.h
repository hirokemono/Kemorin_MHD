
/* init_gl_lighting_c.h*/

#ifndef init_GL_LIGHTING_C_
#define init_GL_LIGHTING_C_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "glsl.h"

/* prototypes */
void set_bg_color_kemoview(struct mesh_menu_val *mesh_m);
void init_bg_color_kemoview(struct mesh_menu_val *mesh_m);

void kemo_gl_initial_lighting_c(struct view_element *view_s, 
								struct kemoview_shaders *kemo_shaders);
void set_gl_3D_lighting_c();
void set_gl_map_lighting_c();
void reset_light_from_white_sf_c(int surface_color);
void reset_light_by_size_of_domain(GLdouble r_max);

#endif
