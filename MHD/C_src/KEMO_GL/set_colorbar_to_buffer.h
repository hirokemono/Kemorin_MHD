/*
// set_colorbar_to_buffer.h
*/

#ifndef SET_COLORBAR_TO_BUFFER__
#define SET_COLORBAR_TO_BUFFER__

#include <math.h>
#include "m_color_table_c.h"
#include "m_vertex_buffer.h"
#include "m_colorbar_buffer.h"
#include "rainbow_color_code_c.h"
#include "set_primitives_to_gl_buffer.h"

/* prototypes */

long solid_colorbar_box_to_buf(const long ist_quad, 
                               struct colormap_params *cmap_s, 
                               struct cbar_work *cbar_wk,
                               struct gl_strided_buffer *strided_buf);
long fade_colorbar_box_to_buf(const long ist_quad,
                              struct colormap_params *cmap_s, float *bg_color,
                              struct cbar_work *cbar_wk,
                              struct gl_strided_buffer *strided_buf);
long colorbar_frame_to_buf(const long ist_quad, int iflag_retina,
                           float *text_color, struct cbar_work *cbar_wk,
                           struct gl_strided_buffer *strided_buf);

void colorbar_mbox_to_buf(int iflag_retina, float *text_color,
                          struct cbar_work *cbar_wk,
                          struct gl_strided_buffer *min_vertex,
                          struct gl_strided_buffer *max_vertex,
                          struct gl_strided_buffer *zero_vertex);
void time_mbox_to_buf(int iflag_retina, float *text_color,
                      float xwin, float ywin,
                      struct gl_strided_buffer *strided_buf);

void message_mbox_to_buf(const int iflag_retina, const float text_opacity,
                         const float xbar_max, const float ybar_min,
                         struct gl_strided_buffer *strided_buf);
void screen_mbox_to_buf(const int npix_x, const int npix_y,
                        struct gl_strided_buffer *strided_buf);

#endif

