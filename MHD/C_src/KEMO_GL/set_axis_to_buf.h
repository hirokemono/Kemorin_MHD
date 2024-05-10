
/* set_axis_to_buf.h*/

#ifndef SET_AXIS_TO_BUF_
#define SET_AXIS_TO_BUF_

#include <math.h>
#include <stdio.h>


#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "modify_object_4_viewer_c.h"
#include "icosahedron_c.h"


/* prototypes */
void set_axis_to_buf(struct view_element *view_s, int iflag_draw_axis,
                     double dist, int ncorner, double radius,
                     struct gl_strided_buffer *strided_buf);
	
#endif
