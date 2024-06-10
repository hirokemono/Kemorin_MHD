/*
 *  m_fline_data_4_viewer_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "m_fline_data_4_viewer_c.h"

struct points_data * init_points_data(void){
    struct points_data *points_d = (struct points_data *) malloc(sizeof(struct points_data));
    if(points_d  == NULL){
        printf("malloc error for points_data \n");
        exit(0);
    }
    return points_d;
};

void alloc_points_node_s(long nnod, struct points_data *points_d){
    points_d->nnod_viz = nnod;
    /* allocate memory  xyzw_viz[node #][direction]*/
    points_d->xyzw_viz = (double *)malloc(IFOUR*points_d->nnod_viz*sizeof(double));
    if(points_d->xyzw_viz  == NULL){
        printf("malloc error for points_d->xyzw_viz \n");
        exit(0);
    }
    
    points_d->inod_viz = (long *)calloc(points_d->nnod_viz,sizeof(long));
    if(points_d->inod_viz  == NULL){
        printf("malloc error for points_d->inod_viz \n");
        exit(0);
    }
    return;
};

static void dealloc_points_node_s(struct points_data *points_d){
    free(points_d->inod_viz);
    free(points_d->xyzw_viz);
}


void alloc_points_field_name_c(long nfield, struct points_data *points_d){
    int i;
    
    points_d->nfield = nfield;
    points_d->ncomp =       (long *)calloc(points_d->nfield,sizeof(long));
    if(points_d->ncomp  == NULL){
        printf("malloc error for points_d->ncomp \n");
        exit(0);
    }
    points_d->istack_comp = (long *)calloc(points_d->nfield+1,sizeof(long));
    if(points_d->istack_comp  == NULL){
        printf("malloc error for points_d->istack_comp \n");
        exit(0);
    }

    points_d->id_coord =    (int *)calloc(points_d->nfield,sizeof(int));
    if(points_d->id_coord  == NULL){
        printf("malloc error for points_d->id_coord \n");
        exit(0);
    }
    
    points_d->data_name = (char **)malloc(points_d->nfield*sizeof(char *));
    if(points_d->data_name  == NULL){
        printf("malloc error for points_d->data_name \n");
        exit(0);
    }

    for (i = 0; i < points_d->nfield; i++) {
        points_d->data_name[i] = (char *)calloc(KCHARA_C, sizeof(char));
        if(points_d->data_name[i]  == NULL){
            printf("malloc error for points_d->data_name[i], %d \n", i);
            exit(0);
        }
    };
};

void alloc_points_field_data_c(struct points_data *points_d){
    /* allocate memory  d_nod[node #][component]*/
    long num = points_d->ncomptot * points_d->nnod_viz;
    points_d->d_nod = (double *)malloc(num*sizeof(double));
    if(points_d->d_nod  == NULL){
        printf("malloc error for points_d->d_nod \n");
        exit(0);
    }
};

void dealloc_points_field_data_c(struct points_data *points_d){
    free(points_d->d_nod);
    free(points_d->ncomp);
    free(points_d->istack_comp);
    free(points_d->id_coord);

    for(int i = 0; i < points_d->nfield; i++) free(points_d->data_name[i]);
    free(points_d->data_name);
};

void alloc_points_color_data(struct points_data *points_d){
    points_d->color_nod = (double *)malloc(4*points_d->nnod_viz*sizeof(double));
    if(points_d->color_nod  == NULL){
        printf("malloc error for points_d->color_nod \n");
        exit(0);
    }
    return;
};
void dealloc_points_color_data(struct points_data *points_d){
    free(points_d->color_nod);
    return;
}

void alloc_points_ave_data(struct points_data *points_d){
    long num;

    /* allocate memory  d_amp[node #][field]*/
    num = points_d->nfield * points_d->nnod_viz;
    points_d->d_amp = (double *)malloc(num*sizeof(double));
    if(points_d->d_amp  == NULL){
        printf("malloc error for psf_s->d_amp \n");
        exit(0);
    }

    
    points_d->amp_min = (double *)calloc(points_d->nfield,sizeof(double));
    if(points_d->amp_min  == NULL){
        printf("malloc error for points_d->amp_min \n");
        exit(0);
    }

    points_d->amp_max = (double *)calloc(points_d->nfield,sizeof(double));
    if(points_d->amp_max  == NULL){
        printf("malloc error for points_d->amp_max \n");
        exit(0);
    }

    points_d->d_min = (double *)calloc(points_d->ncomptot,sizeof(double));
    if(points_d->d_min  == NULL){
        printf("malloc error for points_d->d_min \n");
        exit(0);
    }

    points_d->d_max = (double *)calloc(points_d->ncomptot,sizeof(double));
    if(points_d->d_max  == NULL){
        printf("malloc error for points_d->d_max \n");
        exit(0);
    }

    points_d->d_ave = (double *)calloc(points_d->ncomptot,sizeof(double));
    if(points_d->d_ave  == NULL){
        printf("malloc error for points_d->d_ave \n");
        exit(0);
    }

    points_d->d_rms = (double *)calloc(points_d->ncomptot,sizeof(double));
    if(points_d->d_rms  == NULL){
        printf("malloc error for points_d->d_rms \n");
        exit(0);
    }
    return;
};

static void dealloc_points_ave_data(struct points_data *points_d){
    free(points_d->d_amp);
    
    free(points_d->amp_min);
    free(points_d->amp_max);
    free(points_d->d_min);
    free(points_d->d_max);
    free(points_d->d_rms);
    free(points_d->d_ave);
    return;
}

void deallc_all_points_data(struct points_data *points_d){
    dealloc_points_ave_data(points_d);
    dealloc_points_color_data(points_d);
    
    dealloc_points_field_data_c(points_d);
    dealloc_points_node_s(points_d);
    return;
};






struct fline_data * init_fline_data(void){
    struct fline_data *fline_d = (struct fline_data *) malloc(sizeof(struct fline_data));
    if(fline_d  == NULL){
        printf("malloc error for fline_data \n");
        exit(0);
    }
    return fline_d;
};

void alloc_fline_node_s(long nnod, struct fline_data *fline_d){
    fline_d->nnod_viz = nnod;
    /* allocate memory  xyzw_viz[node #][direction]*/
    fline_d->xyzw_viz = (double *)malloc(IFOUR*fline_d->nnod_viz*sizeof(double));
    if(fline_d->xyzw_viz  == NULL){
        printf("malloc error for fline_d->xyzw_viz \n");
        exit(0);
    }
    
    fline_d->inod_viz = (long *)calloc(fline_d->nnod_viz,sizeof(long));
    if(fline_d->inod_viz  == NULL){
        printf("malloc error for fline_d->inod_viz \n");
        exit(0);
    }
    return;
};

static void dealloc_fline_node_s(struct fline_data *fline_d){
    free(fline_d->inod_viz);
    free(fline_d->xyzw_viz);
}

void alloc_fline_ele_s(long n_ele, long nnod_4_ele,
                       struct fline_data *fline_d){
    int i;
    
    fline_d->nele_viz =       n_ele;
    fline_d->nnod_4_ele_viz = nnod_4_ele;
    /* allocate memory  ie_viz[patch #][connection]*/
    fline_d->ie_viz = (long **) malloc(fline_d->nele_viz*sizeof(long *));
    if(fline_d->ie_viz  == NULL){
        printf("malloc error for fline_d->ie_viz \n");
        exit(0);
    }

    for (i = 0; i < fline_d->nele_viz; i++){
        fline_d->ie_viz[i] = (long *)calloc(fline_d->nnod_4_ele_viz,sizeof(long));
        if(fline_d->ie_viz[i]  == NULL){
            printf("malloc error for fline_d->ie_viz[i], %d \n", i);
            exit(0);
        }
    };

    /* allocate memory  xyzw_ele_viz[patch #][direction]*/
    fline_d->xyzw_ele_viz = (double *) malloc(4*fline_d->nele_viz*sizeof(double));
    if(fline_d->xyzw_ele_viz  == NULL){
        printf("malloc error for fline_d->xyzw_ele_viz \n");
        exit(0);
    }
    return;
};

void dealloc_fline_ele_s(struct fline_data *fline_d){
    free(fline_d->xyzw_ele_viz);
    free(fline_d->ie_viz);
};


void alloc_fline_field_name_c(long nfield, struct fline_data *fline_d){
    int i;
    
    fline_d->nfield = nfield;
    fline_d->ncomp =       (long *)calloc(fline_d->nfield,sizeof(long));
    if(fline_d->ncomp  == NULL){
        printf("malloc error for fline_d->ncomp \n");
        exit(0);
    }

    fline_d->istack_comp = (long *)calloc(fline_d->nfield+1,sizeof(long));
    if(fline_d->istack_comp  == NULL){
        printf("malloc error for fline_d->istack_comp \n");
        exit(0);
    }

    fline_d->id_coord =    (int *)calloc(fline_d->nfield,sizeof(int));
    if(fline_d->id_coord  == NULL){
        printf("malloc error for fline_d->id_coord \n");
        exit(0);
    }
    
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
    long num = fline_d->ncomptot * fline_d->nnod_viz;
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

    fline_d->color_nod = (double *)malloc(4*fline_d->nnod_viz*sizeof(double));
    if(fline_d->color_nod  == NULL){
        printf("malloc error for fline_d->color_nod \n");
        exit(0);
    }

    fline_d->dir_nod = (double *)malloc(4*fline_d->nnod_viz*sizeof(double));
    if(fline_d->dir_nod  == NULL){
        printf("malloc error for fline_d->dir_nod \n");
        exit(0);
    }
    return;
};

void alloc_fline_work_data(struct fline_data *fline_d){
    /* allocate memory  dir_edge[patch #][component]*/
    fline_d->length_edge = (double *)calloc(fline_d->nele_viz,sizeof(double));
    if(fline_d->length_edge  == NULL){
        printf("malloc error for fline_d->length_edge \n");
        exit(0);
    }
    fline_d->dir_edge = (double *)malloc(4*fline_d->nele_viz*sizeof(double));
    if(fline_d->dir_edge  == NULL){
        printf("malloc error for psf_s->dir_edge \n");
        exit(0);
    }
    return;
};

void alloc_fline_ave_data(struct fline_data *fline_d){
    long num;

    /* allocate memory  d_amp[node #][field]*/
    num = fline_d->nfield * fline_d->nnod_viz;
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

