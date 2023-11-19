
/* draw_fieldlines.h */

#ifndef DRAW_FIELDLINES_
#define DRAW_FIELDLINES_

#include "m_kemoview_fline_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "set_fieldline_to_buf.h"

/* prototypes */
void const_fieldlines_buffer(struct psf_data *fline_s, struct fline_menu_val *fline_m,
                             struct gl_strided_buffer *FLINE_tube_buf,
                             struct gl_strided_buffer *FLINE_line_buf);
#endif
