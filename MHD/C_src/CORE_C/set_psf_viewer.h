/*
 *  set_psf_viewer.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/13.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef SET_PSF_VIEWER_
#define SET_PSF_VIEWER_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "skip_comment_c.h"
#include "set_new_patch_4_map_c.h"
#include "take_normal_psf_c.h"

struct map_interpolate{
    long nnod_org;
    long nnod_added_4_map;
    long nele_added_4_map;
    long *inod_org_4_map_itp;
    double *coef_4_map_itp;
};


/* prototypes */
void set_viewer_fieldline_data(struct fline_data *fline_d,
                               struct psf_data *viz_tmp);
long set_viewer_data_with_mapping(struct psf_data *viz_s, struct psf_data *viz_tmp);

#endif
