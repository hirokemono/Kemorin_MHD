/*
 *  cal_viz_field_ranges.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef CAL_VIZ_FIELD_RANGES_
#define CAL_VIZ_FIELD_RANGES_

#include <stdlib.h>
#include "coordinate_converter_c.h"

/* prototypes */

double cal_psf_grid_range(long nnod_viz, double *xyzw_viz,
                          double *xmin_psf, double *xmax_psf,
                          double *center_psf);
void take_minmax_psf_each_component(long nnod_viz, long nfield, long ncomptot,
                                    long *istack_comp, double *d_nod, double *d_amp,
                                    double *d_min, double *d_max);
void take_minmax_viz_fields(long nfield, long *istack_comp,
                            double *d_min, double *d_max,
                            double *amp_min, double *amp_max);

void cal_colat_and_longitude(long nadded_for_phi0,
                             long nnod_viz, double *xyzw_viz, 
                             double *rt_viz);
void shift_longitude(double add_phi, long nnod_viz, double *xyzw_viz);


#endif /*  CAL_VIZ_FIELD_RANGES_  */