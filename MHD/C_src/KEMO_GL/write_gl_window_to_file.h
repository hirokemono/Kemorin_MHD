
/* write_gl_window_to_file.h */

#ifndef WRITE_GL_WINDOW_TO_FILE_
#define WRITE_GL_WINDOW_TO_FILE_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "move_draw_objects_gl.h"
#include "gl_buffer_2_png.h"
#include "gl_buffer_2_bmp.h"


/* prototypes */
int set_image_format_id_by_ext(char *image_fmt);
void write_gl_window_to_file(int iflag_img, const char *fhead, int nwin_x, int nwin_y);
void write_gl_window_step_file(int iflag_img, int istep, const char *fhead,
			int nwin_x, int nwin_y);

#endif
