
/* set_fieldline_to_buf.h */

#ifndef SET_FIELDLINE_TO_BUF_
#define SET_FIELDLINE_TO_BUF_

#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "icosahedron_c.h"

/* prototypes */
int count_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s);
int count_fieldlines_to_buf(struct psf_data *fline_s);

int set_fieldtubes_to_buf(int ncorner, 
			struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct gl_strided_buffer *strided_buf);
int set_fieldlines_to_buf(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct gl_strided_buffer *strided_buf);
#endif
