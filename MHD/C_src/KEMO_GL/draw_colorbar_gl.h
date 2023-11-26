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
void init_bg_color_kemoview(float bg_color[4], float text_color[4]);
void set_bg_color_kemoview(float bg_color[4], float text_color[4]);

int count_colorbar_box_VAO(int iflag_zero, int num_quad);

void const_timelabel_buffer(int iflag_retina, int nx_win, int ny_win,
                            float text_color[4], float bg_color[4],
                            struct kemo_array_control *psf_a,
                            struct line_text_image *tlabel_image,
                            struct gl_strided_buffer *time_buf);
void const_colorbar_buffer(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *min_buf,  struct line_text_image *cbar_min_image,
                           struct gl_strided_buffer *max_buf,  struct line_text_image *cbar_max_image,
                           struct gl_strided_buffer *zero_buf, struct line_text_image *cbar_zero_image,
                           struct gl_strided_buffer *cbar_buf);

void set_time_text_VAO(struct line_text_image *tlabel_image, struct VAO_ids *text_VAO,
                       struct gl_strided_buffer *time_buf);
void set_colorbar_VAO(struct gl_strided_buffer *min_buf,  struct line_text_image *cbar_min_image,
                      struct gl_strided_buffer *max_buf,  struct line_text_image *cbar_max_image,
                      struct gl_strided_buffer *zero_buf, struct line_text_image *cbar_zero_image,
                      struct gl_strided_buffer *cbar_buf, struct VAO_ids **cbar_VAO);
#endif

