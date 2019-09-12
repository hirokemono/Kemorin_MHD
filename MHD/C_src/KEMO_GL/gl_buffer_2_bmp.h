
/* gl_buffer_2_bmp.h */

#ifndef GL_BUFFER_BMP_
#define GL_BUFFER_BMP_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer_param_c.h"
#include "write_image_2_bmp.h"
#include "move_draw_objects_gl.h"

/* prototypes */

void gl_buffer_to_bmp(const char *fhead, int num_x, int num_y);
void gl_buffer_to_ppm_p6(const char *fhead, int num_x, int num_y);
void gl_buffer_to_ppm_p3(const char *fhead, int num_x, int num_y);

#endif

