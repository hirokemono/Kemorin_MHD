/*
 *  m_fline_data_4_viewer_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "m_fline_data_4_viewer_c.h"

struct fline_data * init_fline_data(void){
    struct fline_data *fline_d = (struct fline_data *) malloc(sizeof(struct fline_data));
    if(fline_d  == NULL){
        printf("malloc error for fline_data \n");
        exit(0);
    }
    return fline_d;
};

void alloc_fline_data(long nnod_fline, struct fline_data *fline_d){
    fline_d->nnod_fline = nnod_fline;
    fline_d->dir_nod = (double *)malloc(4*fline_d->nnod_fline*sizeof(double));
    if(fline_d->dir_nod  == NULL){
        printf("malloc error for fline_d->dir_nod \n");
        exit(0);
    }
    return;
};

void dealloc_fline_data(struct fline_data *fline_d){
    free(fline_d->dir_nod);
    return;
}
