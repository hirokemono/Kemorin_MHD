/*
 *  fline_edge_direction_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef FLINE_EDGE_DIRECTION_C_
#define FLINE_EDGE_DIRECTION_C_

#include <stdlib.h>

#include "m_fline_data_4_viewer_c.h"
#include "cal_viz_field_ranges.h"
#include "coordinate_converter_c.h"

/* prototypes */

void take_length_fline(struct psf_data *fline_d,
                       struct fline_directions *fline_dir);
void take_minmax_fline(struct fline_directions *fline_dir,
                       struct psf_data *fline_d);
void take_minmax_points(struct psf_data *point_d);

#endif /* FLINE_EDGE_DIRECTION_C_ */
