
/* draw_colorbar_gl.h */

#ifndef DRAW_COLORBAR_GL_
#define DRAW_COLORBAR_GL_

#include <math.h>
#include "m_gl_transfer_matrix.h"
#include "m_color_table_c.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "ysglusefontbitmap.h"

char colorbar_text[60];
/* prototypes */

void init_colorbar_fonts();
void draw_colorbar_gl(GLint nx_win, GLint ny_win, 
			GLfloat text_color[4], struct colormap_params *cmap_s);

#endif

