/*
// set_colorbar_to_buffer.h
*/

#ifndef SET_COLORBAR_TO_BUFFER__
#define SET_COLORBAR_TO_BUFFER__

#include <math.h>
#include "m_color_table_c.h"
#include "m_colorbar_work.h"
#include "vartex_array_object_gl.h"
#include "rainbow_color_code_c.h"

/* prototypes */

int solid_colorbar_box_to_buf(int ist_quad, struct colormap_params *cmap_s, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf);
int fade_colorbar_box_to_buf(int ist_quad, struct colormap_params *cmap_s, float *bg_color, 
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf);
int colorbar_frame_to_buf(int ist_quad, int iflag_retina, float *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf);
void colorbar_mbox_to_buf(int iflag_retina, float *text_color,
			struct cbar_work *cbar_wk, struct gl_strided_buffer *strided_buf);

void message_mbox_to_buf(int iflag_retina, float *text_color,
                         struct msg_work *msg_wk, struct gl_strided_buffer *strided_buf);

#endif

