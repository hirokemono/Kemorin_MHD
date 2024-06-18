/*
 *  pthread_fieldline_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include <pthread.h>

#include "pthread_fieldline_to_buf.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer *strided_buf;
    struct gl_index_buffer   *index_buf;

    struct psf_data          *fline_d;
    struct fline_directions  *fline_dir;
    struct psf_menu_val      *fline_m;
    double tube_width;

    long *num_patch;
} args_pthread_fieldline;


static void * set_fieldtubes_to_buf_1thread(void *args){
    args_pthread_fieldline * p = (args_pthread_fieldline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct gl_index_buffer *index_buf = p->index_buf;
    
    struct psf_data       *fline_d =   p->fline_d;
    struct fline_directions *fline_dir = p->fline_dir;
    struct psf_menu_val   *fline_m =   p->fline_m;
    double tube_width = p->tube_width;

    long *num_patch =  p->num_patch;
    
    long lo = fline_d->nele_viz * id /     nthreads;
    long hi = fline_d->nele_viz * (id+1) / nthreads;
        
    num_patch[id] = set_fieldtubes_to_buf(lo, lo, hi, tube_width,
                                          fline_d, fline_dir, fline_m,
                                          strided_buf, index_buf);
    return 0;
}

static void * set_fieldlines_to_buf_1thread(void *args){
    args_pthread_fieldline * p = (args_pthread_fieldline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct psf_data     *fline_d = p->fline_d;
    struct psf_menu_val *fline_m = p->fline_m;
    
    long *num_patch =  p->num_patch;
    
    long lo = fline_d->nele_viz * id /     nthreads;
    long hi = fline_d->nele_viz * (id+1) / nthreads;
    num_patch[id] = set_fieldlines_to_buf(lo, lo, hi, fline_d, fline_m,
                                          strided_buf);
    return 0;
}


static long set_fieldtubes_to_buf_pthread(long ist_patch, const int nthreads,
                                          double tube_width,
                                          struct psf_data *fline_d,
                                          struct fline_directions *fline_dir,
                                          struct psf_menu_val *fline_m,
                                          struct gl_strided_buffer *strided_buf,
                                          struct gl_index_buffer   *index_buf){
/* Allocate thread arguments. */
    args_pthread_fieldline *args
                = (args_pthread_fieldline *) malloc (nthreads * sizeof(args_pthread_fieldline));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_fieldline.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = strided_buf;
        args[ip].index_buf =   index_buf;

        args[ip].fline_d =   fline_d;
        args[ip].fline_dir = fline_dir;
        args[ip].fline_m =   fline_m;
        args[ip].tube_width =  tube_width;

        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, set_fieldtubes_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}

static long set_fieldlines_to_buf_pthread(long ist_patch, const int nthreads, 
                                          struct psf_data *fline_d,
                                          struct psf_menu_val *fline_m,
                                          struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_fieldline *args
                = (args_pthread_fieldline *) malloc (nthreads * sizeof(args_pthread_fieldline));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_fieldline.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = strided_buf;
        
        args[ip].fline_d = fline_d;
        args[ip].fline_m = fline_m;

        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, set_fieldlines_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}


long sel_fieldtubes_to_buf_pthread(long ist_patch, const int nthreads,
                                   double tube_width,
                                   struct psf_data *fline_d,
                                   struct fline_directions *fline_dir,
                                   struct psf_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf,
                                   struct gl_index_buffer   *index_buf){
    long num_tube = ist_patch;
    if(nthreads > 1){
        num_tube = set_fieldtubes_to_buf_pthread(num_tube, nthreads, tube_width,
                                                 fline_d, fline_dir, fline_m,
                                                 strided_buf, index_buf);
    }else{
        num_tube = set_fieldtubes_to_buf(num_tube, IZERO, fline_d->nele_viz,
                                         tube_width, fline_d, fline_dir,
                                         fline_m, strided_buf, index_buf);
    };
    return num_tube;
}

long sel_fieldlines_to_buf_pthread(long ist_patch, const int nthreads, 
                                   struct psf_data *fline_d,
                                   struct psf_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_fieldlines_to_buf_pthread(num_patch, nthreads,
                                                  fline_d, fline_m, strided_buf);
    }else{
        num_patch = set_fieldlines_to_buf(num_patch, IZERO, fline_d->nele_viz,
                                          fline_d, fline_m, strided_buf);
    };
    return num_patch;
}
