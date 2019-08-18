
/* draw_axis_c.h*/

#ifndef DRAW_AXIS_C_
#define DRAW_AXIS_C_

#include <math.h>
#include <stdio.h>


#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "modify_object_4_viewer_c.h"
#include "set_axis_to_buf.h"


/* prototypes */
void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf);
	
#endif
