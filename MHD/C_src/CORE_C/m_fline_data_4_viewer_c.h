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
    long nnod_4_edge_fline;
    long **iedge_fline;

    
    double *xyzw_edge_fline;
    double *dir_edge;
    double *length_edge;
    double length_total;
    
    long nnod_fline;
    long *inod_fline;
    double *xyzw_fline;

    long nfield;
    long *ncomp;
    long *istack_comp;
    int *id_coord;
    char **data_name;
    
    long ncomptot;
    double *d_nod;
    
    double *d_amp;
    double *color_nod;
    
    double *dir_nod;
    
    double *d_min;
    double *d_max;
    double *d_ave;
    double *d_rms;
    
    double *amp_min;
    double *amp_max;
    
    double center_psf[3];
    double xmin_psf[3];
    double xmax_psf[3];
    double rmax_psf;
};

/* prototypes */

struct fline_data * init_fline_data(void);

void alloc_fline_node_s(long nnod, struct fline_data *fline_d);

void alloc_fline_ele_s(long n_ele, long nnod_4_ele,
                       struct fline_data *fline_d);

void alloc_fline_field_name_c(long nfield, struct fline_data *fline_d);
void alloc_fline_field_data_c(struct fline_data *fline_d);

void alloc_fline_data(struct fline_data *fline_d);
void alloc_fline_work_data(struct fline_data *fline_d);
void alloc_fline_ave_data(struct fline_data *fline_d);

void dealloc_fline_work_data(struct fline_data *fline_d);
void deallc_all_fline_data(struct fline_data *fline_d);

#endif  /* M_FLINE_DATA_4_VIEWER_C_ */
