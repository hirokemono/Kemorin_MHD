
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

void initialize_gl_shaders(struct kemoview_shaders *kemo_shaders);
void init_gl_menu_setup(struct kemoview_shaders *kemo_shaders);

void colormap_to_glsl(const int id_cmap, const int num_cmap, const int num_alpha, 
                      const float table_ref[16], const float table_nrm[16],
                      const float alpha_ref[16], const float alpha_nrm[16],
                      struct shader_ids *phong_w_cmap);
#endif
