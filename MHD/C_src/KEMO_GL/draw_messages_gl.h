/*
// draw_messages_gl.h
*/

#ifndef DRAW_MESSAGES_GL_
#define DRAW_MESSAGES_GL_

#include <math.h>
#include "m_kemoview_psf_menu.h"
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "set_colorbar_to_buffer.h"


/* prototypes */

void set_message_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], struct msg_work *msg_wk, 
			struct VAO_ids *msg_VAO);
void draw_message_VAO(struct msg_work *msg_wk,
			struct VAO_ids *msg_VAO, struct kemoview_shaders *kemo_shaders);

#endif

