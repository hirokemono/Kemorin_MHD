
/* read_psf_data_gz_c.h */

#ifndef READ_PSF_DATA_GZ_C_
#define READ_PSF_DATA_GZ_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "calypso_zlib_io_c.h"
#include "skip_comment_c.h"

/* prototypes */
int read_kemoview_ucd_gz(const char *file_name, struct psf_data *viz_s);

int read_psf_grd_gz(const char *file_name, struct psf_data *viz_s);
int read_psf_udt_gz(const char *file_name, struct psf_data *viz_s);
#endif
