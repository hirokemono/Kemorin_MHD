
/* gl_buffer_2_bmp.h */

#ifndef GL_BUFFER_BMP_
#define GL_BUFFER_BMP_

#include <stdio.h>
#include <stdlib.h>

#include "kemoviewer_param_c.h"
#include "write_image_2_bmp.h"

/* prototypes */

void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img);
void flip_gl_bitmap_to_img2d(int num_x, int num_y,
                             unsigned char *glimage, unsigned char **img_2d);

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage);

void gl_buffer_to_bmp(const char *fhead, int num_x, int num_y);
void gl_buffer_to_ppm_p6(const char *fhead, int num_x, int num_y);
void gl_buffer_to_ppm_p3(const char *fhead, int num_x, int num_y);

#endif

