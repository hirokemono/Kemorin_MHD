/*
 *  m_fline_data_4_viewer_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "m_fline_data_4_viewer_c.h"

void deallc_all_points_data(struct psf_data *points_d){
    dealloc_psf_data_s(points_d);
    dealloc_psf_color_data_c(points_d);

    dealloc_psf_field_data_c(points_d);
    free(points_d->inod_viz);
    free(points_d->xyzw_viz);
    return;
};




struct fline_directions * init_fline_directions(void){
    struct fline_directions *fline_dir = (struct fline_directions *) malloc(sizeof(struct fline_directions));
    if(fline_dir  == NULL){
        printf("malloc error for fline_directions \n");
        exit(0);
    }
    return fline_dir;
};

void alloc_fline_direction_data(struct psf_data *fline_d,
                                struct fline_directions *fline_dir){
    fline_dir->dir_nod = (double *)malloc(4*fline_d->nnod_viz*sizeof(double));
    if(fline_dir->dir_nod  == NULL){
        printf("malloc error for fline_dir->dir_nod \n");
        exit(0);
    }
    return;
};

void alloc_fline_work_data(struct psf_data *fline_d,
                           struct fline_directions *fline_dir){
    /* allocate memory  dir_edge[patch #][component]*/
    fline_dir->length_edge = (double *)calloc(fline_d->nele_viz,sizeof(double));
    if(fline_dir->length_edge  == NULL){
        printf("malloc error for fline_dir->length_edge \n");
        exit(0);
    }
    fline_dir->dir_edge = (double *)malloc(4*fline_d->nele_viz*sizeof(double));
    if(fline_dir->dir_edge  == NULL){
        printf("malloc error for psf_s->dir_edge \n");
        exit(0);
    }
    return;
};

void dealloc_fline_work_data(struct fline_directions *fline_dir){
    free(fline_dir->dir_edge);
    free(fline_dir->length_edge);
    return;
}

void dealloc_fline_direction_data(struct fline_directions *fline_dir){
    free(fline_dir->dir_nod);
    return;
}

