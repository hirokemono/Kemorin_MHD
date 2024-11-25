
/* set_axis_to_buf.h*/

#ifndef SET_AXIS_TO_BUF_
#define SET_AXIS_TO_BUF_

#include <math.h>
#include <stdio.h>


#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "modify_object_4_viewer_c.h"
#include "set_primitives_to_gl_buffer.h"
#include "transfer_matvec.h"
#include "invert_small_matrix_c.h"



/* prototypes */
double set_tube_radius_by_axis(struct view_element *view_s);

void set_flex_axis_to_buf(struct view_element *view_s,
                          int iflag_draw_axis, double dist, double radius,
                          struct gl_strided_buffer *strided_buf,
                          struct gl_index_buffer *index_buf);

void set_lower_flex_axis_to_buf(struct view_element *view_s,
                                int iflag_draw_axis, double dist_mesh, double radius_ref,
                                struct gl_strided_buffer *strided_buf,
                                struct gl_index_buffer *index_buf);
void set_lower_fixed_axis_to_buf(struct view_element *view_s,
                                 int iflag_draw_axis, double dist, double radius,
                                 struct gl_strided_buffer *strided_buf,
                                 struct gl_index_buffer *index_buf);

#endif
