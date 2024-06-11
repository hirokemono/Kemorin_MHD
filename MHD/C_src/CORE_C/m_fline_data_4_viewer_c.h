/*
 *  m_fline_data_4_viewer_c.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef M_FLINE_DATA_4_VIEWER_C_
#define M_FLINE_DATA_4_VIEWER_C_

#include <stdlib.h>
#include <stdio.h>
#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"

struct fline_directions{
    double *dir_nod;
    double *dir_edge;
    double *length_edge;
    double total_length;
};


/* prototypes */

void deallc_all_points_data(struct psf_data *points_d);


void alloc_points_color_data(struct psf_data *points_d);
void deallc_all_points_data(struct psf_data *points_d);


struct fline_directions * init_fline_directions(void);
void alloc_fline_direction_data(struct psf_data *fline_d,
                                struct fline_directions *fline_dir);
void alloc_fline_work_data(struct psf_data *fline_d,
                           struct fline_directions *fline_dir);
void dealloc_fline_direction_data(struct fline_directions *fline_dir);
void dealloc_fline_work_data(struct fline_directions *fline_dir);

#endif  /* M_FLINE_DATA_4_VIEWER_C_ */
