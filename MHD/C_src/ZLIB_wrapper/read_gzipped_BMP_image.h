
/* read_gzipped_BMP_image.h */

#ifndef READ_GZIPPED_BMP_IMAGE_
#define READ_GZIPPED_BMP_IMAGE_

#include <stdio.h>
#include <stdlib.h>

#include "calypso_param_c.h"
#include "read_image_2_bmp.h"
#include "calypso_zlib_io_c.h"

/* prototypes */
struct BMP_data * read_gzipped_BMP_c(const char *fhead);

#endif /* READ_GZIPPED_BMP_IMAGE_ */
