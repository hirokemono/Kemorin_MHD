
/* set_fieldline_to_buf.h */

#ifndef SET_FIELDLINE_TO_BUF_
#define SET_FIELDLINE_TO_BUF_

#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "set_primitives_to_gl_buffer.h"




/* prototypes */
long count_fieldlines_to_buf(struct fline_data *fline_d);

long set_fieldtubes_to_buf(long ist_patch, long ist_line, long ied_line,
                           double tube_width,
                           struct fline_data *fline_d,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf);
long set_fieldlines_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct fline_data *fline_d,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf);

#endif
