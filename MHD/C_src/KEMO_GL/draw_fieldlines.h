
/* draw_fieldlines.h */

#ifndef DRAW_FIELDLINES_
#define DRAW_FIELDLINES_

#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "set_fieldline_to_buf.h"

/* prototypes */
void sel_fieldlines_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct VAO_ids *fline_VAO);
void draw_fieldlines_VAO(struct fline_menu_val *fline_m, struct view_element *view_s, 
			struct VAO_ids *fline_VAO, struct kemoview_shaders *kemo_shaders);
#endif
