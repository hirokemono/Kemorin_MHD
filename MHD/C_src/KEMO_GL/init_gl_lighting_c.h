
/* init_gl_lighting_c.h*/

#ifndef init_GL_LIGHTING_C_
#define init_GL_LIGHTING_C_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "glsl.h"
#include "m_phong_light_table_c.h"

/* prototypes */
void set_bg_color_kemoview(struct mesh_menu_val *mesh_m);
void init_bg_color_kemoview(struct mesh_menu_val *mesh_m);

void kemo_gl_initial_lighting_c(struct view_element *view_s, 
								struct kemoview_shaders *kemo_shaders);
#endif
