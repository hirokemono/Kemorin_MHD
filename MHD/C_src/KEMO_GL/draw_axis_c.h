
/* draw_axis_c.h*/

#ifndef DRAW_AXIS_C_
#define DRAW_AXIS_C_

#include <math.h>
#include <stdio.h>


#include "kemoviewer_param_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "modify_object_4_viewer_c.h"


/* prototypes */
void draw_axis(struct view_element *view_s, GLfloat dist);

void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf);
	
#endif
