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

void alloc_fline_node_s(long nnod, struct fline_data *fline_d){
    fline_d->nnod_fline = nnod;
    /* allocate memory  xyzw_fline[node #][direction]*/
    fline_d->xyzw_fline = (double *)malloc(IFOUR*fline_d->nnod_fline*sizeof(double));
    if(fline_d->xyzw_fline  == NULL){
        printf("malloc error for fline_d->xyzw_viz \n");
        exit(0);
    }
    
    fline_d->inod_fline = (long *)calloc(fline_d->nnod_fline,sizeof(long));
    if(fline_d->inod_fline  == NULL){
        printf("malloc error for fline_d->inod_fline \n");
        exit(0);
    }
    return;
};

static void dealloc_fline_node_s(struct fline_data *fline_d){
    free(fline_d->inod_fline);
    free(fline_d->xyzw_fline);
}

void alloc_fline_ele_s(long n_ele, long nnod_4_ele,
                       struct fline_data *fline_d){
    int i;
    
    fline_d->nedge_fline =       n_ele;
    fline_d->nnod_4_edge_fline = nnod_4_ele;
    /* allocate memory  ie_viz[patch #][connection]*/
    fline_d->iedge_fline = (long **) malloc(fline_d->nedge_fline*sizeof(long *));
    if(fline_d->iedge_fline  == NULL){
        printf("malloc error for fline_d->iedge_fline \n");
        exit(0);
    }

    for (i = 0; i < fline_d->nedge_fline; i++){
        fline_d->iedge_fline[i] = (long *)calloc(fline_d->nnod_4_edge_fline,sizeof(long));
        if(fline_d->iedge_fline[i]  == NULL){
            printf("malloc error for fline_d->iedge_fline[i], %d \n", i);
            exit(0);
        }
    };
    return;
};

void dealloc_fline_ele_s(struct fline_data *fline_d){
    free(fline_d->iedge_fline);
};


void alloc_fline_field_name_c(long nfield, struct fline_data *fline_d){
    int i;
    
    fline_d->nfield = nfield;
    fline_d->ncomp =       (long *)calloc(fline_d->nfield,sizeof(long));
    fline_d->istack_comp = (long *)calloc(fline_d->nfield+1,sizeof(long));

    fline_d->id_coord =    (int *)calloc(fline_d->nfield,sizeof(int));
    
    fline_d->data_name = (char **)malloc(fline_d->nfield*sizeof(char *));
    if(fline_d->data_name  == NULL){
        printf("malloc error for fline_d->data_name \n");
        exit(0);
    }

    for (i = 0; i < fline_d->nfield; i++) {
        fline_d->data_name[i] = (char *)calloc(KCHARA_C, sizeof(char));
        if(fline_d->data_name[i]  == NULL){
            printf("malloc error for fline_d->data_name[i], %d \n", i);
            exit(0);
        }
    };
};

void alloc_fline_field_data_c(struct fline_data *fline_d){
    /* allocate memory  d_nod[node #][component]*/
    long num = fline_d->ncomptot * fline_d->nnod_fline;
    fline_d->d_nod = (double *)malloc(num*sizeof(double));
    if(fline_d->d_nod  == NULL){
        printf("malloc error for fline_d->d_nod \n");
        exit(0);
    }
};
void dealloc_fline_field_data_c(struct fline_data *fline_d){
    free(fline_d->d_nod);
    free(fline_d->ncomp);
    free(fline_d->istack_comp);
    free(fline_d->id_coord);

    for(int i = 0; i < fline_d->nfield; i++) free(fline_d->data_name[i]);
    free(fline_d->data_name);
};

void alloc_fline_data(struct fline_data *fline_d){

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

    /* allocate memory  xyzw_ele_viz[patch #][direction]*/
    fline_d->xyzw_edge_fline = (double *) malloc(4*fline_d->nedge_fline*sizeof(double));
    if(fline_d->xyzw_edge_fline  == NULL){
        printf("malloc error for fline_d->xyzw_edge_fline \n");
        exit(0);
    }
    return;
};

void alloc_fline_work_data(struct fline_data *fline_d){
    /* allocate memory  dir_edge[patch #][component]*/
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

void alloc_fline_ave_data(struct fline_data *fline_d){
    long num;

    /* allocate memory  d_amp[node #][field]*/
    num = fline_d->nfield * fline_d->nnod_fline;
    fline_d->d_amp = (double *)malloc(num*sizeof(double));
    if(fline_d->d_amp  == NULL){
        printf("malloc error for psf_s->d_amp \n");
        exit(0);
    }

    
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
    free(fline_d->xyzw_edge_fline);
    return;
}

static void dealloc_fline_ave_data(struct fline_data *fline_d){
    free(fline_d->d_amp);
    
    free(fline_d->amp_min);
    free(fline_d->amp_max);
    free(fline_d->d_min);
    free(fline_d->d_max);
    free(fline_d->d_rms);
    free(fline_d->d_ave);
    return;
}

void deallc_all_fline_data(struct fline_data *fline_d){
    dealloc_fline_ave_data(fline_d);
    dealloc_fline_data(fline_d);
    
    dealloc_fline_ele_s(fline_d);
    dealloc_fline_field_data_c(fline_d);
    dealloc_fline_node_s(fline_d);
    return;
};

