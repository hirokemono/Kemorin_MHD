
/* read_image_2_bmp.h */

#ifndef READ_IMAGE_2_BMP_
#define READ_IMAGE_2_BMP_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "calypso_zlib_io_c.h"

struct BMP_data{
    int ihpixf;
    int jvpixf;
    int npixel;
    unsigned char *bgr;
};

/* prototypes */
void alloc_bgr_data(struct BMP_data *d_BMP);
struct BMP_data * init_BMP_data(void);
void dealloc_BMP_data(struct BMP_data *d_BMP);


struct BMP_data * read_BMP_c(const char *fhead);
struct BMP_data * read_gzipped_BMP_c(const char *fhead);
void copy_rgb_from_BMP_c(struct BMP_data *d_BMP, unsigned char *image);
void copy_rgba_from_BMP_c(struct BMP_data *d_BMP, unsigned char *image);

#endif
