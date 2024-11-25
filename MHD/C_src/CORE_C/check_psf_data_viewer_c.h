
/* check_psf_data_viewer_c.h */

#ifndef CHECK_PSF_DATA_VIEWER_C_
#define CHECK_PSF_DATA_VIEWER_C_

#include <stdio.h>
#include <stdlib.h>

#include "m_psf_data_4_viewer_c.h"
#include "m_fline_data_4_viewer_c.h"

/* prototypes */

void check_psf_data_c(struct psf_data *tako,
                      struct psf_normals *psf_n);
void check_psf_ave_rms_c(struct psf_data *tako,
                         struct psf_normals *psf_n);
void check_psf_min_max_c(struct psf_data *psf_s);

void check_fline_min_max_c(struct psf_data *psf_s,
                           struct psf_data *fline_d);

#endif
