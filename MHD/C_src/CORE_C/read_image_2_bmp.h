
/* read_image_2_bmp.h */

#ifndef READ_IMAGE_2_BMP_
#define READ_IMAGE_2_BMP_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"

/* prototypes */
int read_BMP_c(const char *fhead, int *ihpixf, int *jvpixf);
void copy_rgb_from_BMP_c(int ihpixf, int jvpixf, unsigned char *image);
void copy_rgba_from_BMP_c(int ihpixf, int jvpixf, unsigned char *image);

#endif
