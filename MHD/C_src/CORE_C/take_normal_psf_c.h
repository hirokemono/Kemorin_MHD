
/* take_normal_psf_c.h */

#ifndef TAKE_NORMAL_PSF_C_
#define TAKE_NORMAL_PSF_C_

#include <math.h>
#include <stdio.h>

#include "m_psf_data_4_viewer_c.h"
#include "cal_surface_center_normal_c.h"
#include "cal_viz_field_ranges.h"

/* prototype */
void easy_normal_nod_psf(struct psf_data *viz_s,
                         struct psf_normals *psf_n);

void take_normal_psf(long nadded_for_phi0,
                     struct psf_data *viz_s,
                     struct psf_normals *psf_n);
void take_minmax_psf(struct psf_data *viz_s,
                     struct psf_normals *psf_n);
#endif
