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
#include "drawGL_by_VAO.h"
#include "coordinate_converter_c.h"
#include "rainbow_color_code_c.h"
#include "set_colorbar_to_buffer.h"


/* prototypes */

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct msg_work *msg_wk,
                          struct gl_strided_buffer *cbar_buf);
void set_message_VAO(int iflag_retina, int nx_win, int ny_win,
                     struct msg_work *msg_wk, struct VAO_ids *msg_VAO,
                     struct gl_strided_buffer *cbar_buf);
void draw_message_VAO(struct msg_work *msg_wk, struct VAO_ids *msg_VAO, 
                      struct transfer_matrices *matrices, 
                      struct kemoview_shaders *kemo_shaders);

#endif

