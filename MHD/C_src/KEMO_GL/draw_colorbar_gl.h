/*
// draw_colorbar_gl.h
*/

#ifndef DRAW_COLORBAR_GL_
#define DRAW_COLORBAR_GL_

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
int count_colorbar_box_VAO(int iflag_zero, int num_quad);

void const_colorbar_buffer(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *cbar_buf);
void const_cbar_text_buffer(int iflag_retina,  float text_color[4],
                            struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *min_buf, struct gl_strided_buffer *max_buf, 
                            struct gl_strided_buffer *zero_buf);
void const_timelabel_buffer(int iflag_retina, int nx_win, int ny_win,
                            float text_color[4], float bg_color[4],
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *time_buf);

void set_colorbar_VAO(int iflag_retina, int nx_win, int ny_win,
                      float text_color[4], float bg_color[4],
                      struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                      struct gl_strided_buffer *cbar_buf, struct gl_strided_buffer *min_buf,
                      struct gl_strided_buffer *max_buf, struct gl_strided_buffer *zero_buf,
					  struct VAO_ids **cbar_VAO);
void set_timelabel_VAO(int iflag_retina, int nx_win, int ny_win,
                       float text_color[4], float bg_color[4],
                       struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                       struct gl_strided_buffer *time_buf, struct VAO_ids *time_VAO);
#endif

