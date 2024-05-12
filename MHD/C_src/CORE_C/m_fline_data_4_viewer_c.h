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

struct fline_data{
    long nedge_fline;
    double *dir_edge;
    double *length_edge;
    double length_total;

    long nnod_fline;
	double *dir_nod;
    
    long ncomptot;
    double *d_min;
    double *d_max;
    double *d_ave;
    double *d_rms;
    
    long nfield;
    double *amp_min;
    double *amp_max;
};

/* prototypes */

struct fline_data * init_fline_data(void);
void alloc_fline_data(long nnod_fline, struct fline_data *fline_d);
void alloc_fline_work_data(long nedge_fline, struct fline_data *fline_d);
void alloc_fline_ave_data(long nfield, long ncomptot,
                          struct fline_data *fline_d);

void dealloc_fline_work_data(struct fline_data *fline_d);
void deallc_all_fline_data(struct psf_data *psf_s,
                           struct fline_data *fline_d);

#endif  /* M_FLINE_DATA_4_VIEWER_C_ */
