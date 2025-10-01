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
#include "cal_viz_field_ranges.h"

struct map_interpolate{
    long nnod_org;
    long nnod_added_4_map;
    long nele_added_4_map;
    long *inod_org_4_map_itp;
    double *coef_4_map_itp;
};

/* prototypes */

struct map_interpolate * alloc_psf_cutting_4_map(void);
void alloc_psf_cutting_4_map_item(struct map_interpolate *map_itp);
void dealloc_psf_cutting_4_map(struct map_interpolate *map_itp);

void set_viewer_points_data(struct psf_data *points_d,
                            struct psf_data *viz_tmp);
void set_viewer_fieldline_data(struct psf_data *fline_d,
                               struct psf_data *viz_tmp);
long set_viewer_mesh_with_mapping(struct map_interpolate *map_itp,
                                  struct psf_data *viz_s,
                                  struct psf_data *viz_tmp);
void set_viewer_data_with_mapping(struct map_interpolate *map_itp,
                                  struct psf_data *viz_tmp, struct psf_data *viz_s);

#endif
