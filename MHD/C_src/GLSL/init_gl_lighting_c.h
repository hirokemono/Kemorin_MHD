
/* init_gl_lighting_c.h*/

#ifndef init_GL_LIGHTING_C_
#define init_GL_LIGHTING_C_

#include "glsl.h"
#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "m_phong_light_table_c.h"

/* prototypes */
void set_gl_bg_color(float bg_color[4]);

void kemo_gl_initial_lighting_c(struct kemoview_shaders *kemo_shaders);
void init_gl_menu_setup(struct kemoview_shaders *kemo_shaders);

#endif
