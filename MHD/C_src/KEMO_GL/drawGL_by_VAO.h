
/* drawGL_by_VAO.h */

#include <math.h>

#include "kemoviewer.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"

#ifndef DRAWGL_BY_VAO_
#define DRAWGL_BY_VAO_

/* prototypes */

void drawgl_patch_with_phong(struct view_element *view_s, struct VAO_ids *VAO, 
			struct kemoview_shaders *kemo_shaders);
void drawgl_lines(struct view_element *view_s, 
			struct VAO_ids *VAO, struct kemoview_shaders *kemo_shaders);

#endif
