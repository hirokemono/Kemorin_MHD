
/* write_image_2_png.h */

#ifndef WRITE_IMAGE_2_PNG_
#define WRITE_IMAGE_2_PNG_

#include <stdio.h>
#include <stdlib.h>

#include "zlib.h" 
#include "png.h"
#include "kemosrc_param_c.h"

/* prototypes */


void write_png_rgba(const char *file_name, png_uint_32 num_x, png_uint_32 num_y,
			png_bytepp image);
void write_png_rgb(const char *file_name, png_uint_32 num_x, png_uint_32 num_y,
			png_bytepp image);

void write_png_rgba_c(const char *fhead, int *num_x, int *num_y, 
			const unsigned char *cimage);
void write_png_rgb_c(const char *fhead, int *num_x, int *num_y, 
			const unsigned char *cimage);

/*    file_name:     output file_name
 num_x, num_y:  size of image (pixels)
 image:         image array
 (unsigned char, 4*num_x*num_y for RGBA, 3*num_x*num_y for RGB) */

#endif

