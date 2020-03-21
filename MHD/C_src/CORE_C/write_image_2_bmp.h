
/* write_image_2_bmp.h */

#ifndef WRITE_IMAGE_2_BMP_
#define WRITE_IMAGE_2_BMP_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"

/* prototypes */
void cvt_8bit_cl_int_2_chara(int ihpixf, int jvpixf, int *icl_tbl, unsigned char *rgb);
int pixout_BMP_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb);
int pixout_ppm_p3_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb);
int pixout_ppm_p6_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb);

#endif
