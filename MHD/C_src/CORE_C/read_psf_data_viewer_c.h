
/* read_psf_data_viewer_c.h */

#ifndef READ_PSF_DATA_VIEWER_C_
#define READ_PSF_DATA_VIEWER_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemosrc_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "skip_comment_c.h"

/* prototypes */
int read_kemoview_ucd(const char *file_head, struct psf_data *viz_s);
int read_psf_grd(const char *file_head, struct psf_data *viz_s);
int read_psf_udt(const char *file_head, int istep, struct psf_data *viz_s);

#endif
