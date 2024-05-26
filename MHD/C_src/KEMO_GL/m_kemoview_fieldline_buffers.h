
/* m_kemoview_fieldline_buffers.h */

#ifndef M_KEMOVIEW_FIELDLINE_BUFFERS_
#define M_KEMOVIEW_FIELDLINE_BUFFERS_

#include "m_kemoview_fline_menu.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_color_code_on_nodes.h"
#include "set_fieldline_to_buf.h"
#include "pthread_fieldline_to_buf.h"
#include "set_axis_to_buf.h"

struct FieldLine_buffers{
    struct gl_strided_buffer *FLINE_line_buf;
    struct gl_strided_buffer *FLINE_tube_buf;
};

/* prototypes */
struct FieldLine_buffers * init_FieldLine_buffers(void);
void dealloc_FieldLine_buffers(struct FieldLine_buffers *Fline_bufs);

void const_fieldlines_buffer(const int nthreads, struct view_element *view_s,
                             struct fline_data *fline_d, struct fline_menu_val *fline_m,
                             struct FieldLine_buffers *Fline_bufs);
#endif   /* M_KEMOVIEW_FIELDLINE_BUFFERS_ */
