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

    fline_d->color_nod = (double *)malloc(4*fline_d->nnod_fline*sizeof(double));
    if(fline_d->color_nod  == NULL){
        printf("malloc error for fline_d->color_nod \n");
        exit(0);
    }

    fline_d->dir_nod = (double *)malloc(4*fline_d->nnod_fline*sizeof(double));
    if(fline_d->dir_nod  == NULL){
        printf("malloc error for fline_d->dir_nod \n");
        exit(0);
    }
    return;
};

void alloc_fline_work_data(long nedge_fline, struct fline_data *fline_d){
    /* allocate memory  dir_edge[patch #][component]*/
    fline_d->nedge_fline = nedge_fline;
    fline_d->length_edge = (double *)calloc(fline_d->nedge_fline,sizeof(double));
    if(fline_d->length_edge  == NULL){
        printf("malloc error for fline_d->length_edge \n");
        exit(0);
    }
    fline_d->dir_edge = (double *)malloc(4*fline_d->nedge_fline*sizeof(double));
    if(fline_d->dir_edge  == NULL){
        printf("malloc error for psf_s->dir_edge \n");
        exit(0);
    }
    return;
};

void alloc_fline_ave_data(long nfield, long ncomptot,
                          struct fline_data *fline_d){
    fline_d->nfield = nfield;
    fline_d->amp_min = (double *)calloc(fline_d->nfield,sizeof(double));
    if(fline_d->amp_min  == NULL){
        printf("malloc error for fline_d->amp_min \n");
        exit(0);
    }

    fline_d->amp_max = (double *)calloc(fline_d->nfield,sizeof(double));
    if(fline_d->amp_max  == NULL){
        printf("malloc error for fline_d->amp_max \n");
        exit(0);
    }

    fline_d->ncomptot = ncomptot;
    fline_d->d_min = (double *)calloc(fline_d->ncomptot,sizeof(double));
    if(fline_d->d_min  == NULL){
        printf("malloc error for fline_d->d_min \n");
        exit(0);
    }

    fline_d->d_max = (double *)calloc(fline_d->ncomptot,sizeof(double));
    if(fline_d->d_max  == NULL){
        printf("malloc error for fline_d->d_max \n");
        exit(0);
    }

    fline_d->d_ave = (double *)calloc(fline_d->ncomptot,sizeof(double));
    if(fline_d->d_ave  == NULL){
        printf("malloc error for fline_d->d_ave \n");
        exit(0);
    }

    fline_d->d_rms = (double *)calloc(fline_d->ncomptot,sizeof(double));
    if(fline_d->d_rms  == NULL){
        printf("malloc error for fline_d->d_rms \n");
        exit(0);
    }
    return;
};

void dealloc_fline_work_data(struct fline_data *fline_d){
    free(fline_d->dir_edge);
    free(fline_d->length_edge);
    return;
}

static void dealloc_fline_data(struct fline_data *fline_d){
    free(fline_d->color_nod);
    free(fline_d->dir_nod);
    return;
}

static void dealloc_fline_ave_data(struct fline_data *fline_d){
    free(fline_d->amp_min);
    free(fline_d->amp_min);
    free(fline_d->d_min);
    free(fline_d->d_max);
    free(fline_d->d_rms);
    free(fline_d->d_ave);
    return;
}

void deallc_all_fline_data(struct psf_data *psf_s,
                           struct fline_data *fline_d){
    dealloc_fline_ave_data(fline_d);
    dealloc_fline_data(fline_d);

    dealloc_psf_data_s(psf_s);
    dealloc_psf_mesh_c(psf_s);
    return;
};

