
/* gl_buffer_2_png.h */

#ifndef GL_BUFFER_2_PNG_C_
#define GL_BUFFER_2_PNG_C_

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "kemoviewer_param_c.h"
#include "write_image_2_png.h"
#include "move_draw_objects_gl.h"
#include "set_texture_4_psf.h"

/* prototypes */

unsigned char ** alloc_img_buffer_2_png_rgba(int num_x, int num_y);
unsigned char ** alloc_img_buffer_2_png_rgb(int num_x, int num_y);
void dealloc_img_buffer_2_png(int num_y, unsigned char **image);

void get_gl_buffer_for_png(int num_x, int num_y, unsigned char **image);
void gl_buffer_2_png(const char *fhead, const int num_x, const int num_y,
                     unsigned char **image);

#endif
