
/* gl_buffer_2_png.h */

#ifndef GL_BUFFER_2_PNG_C_
#define GL_BUFFER_2_PNG_C_

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "gl_buffer_2_bmp.h"
#include "write_image_2_png.h"
#include "move_draw_objects_gl.h"
#include "set_texture_4_psf.h"

/* prototypes */

void alloc_img_buffer_2_png_rgba(int num_x, int num_y);
void alloc_img_buffer_2_png_rgb(int num_x, int num_y);
void link_img_buffer_4_png(unsigned char **image_p);
void dealloc_img_buffer_2_png(int num_y);

void gl_buffer_2_png(const char *fhead, int num_x, int num_y);

#endif
