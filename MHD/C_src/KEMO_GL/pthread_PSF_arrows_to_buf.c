/*
 *  pthread_PSF_arrows_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include <pthread.h>

#include "pthread_PSF_arrows_to_buf.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct psf_data     *psf_s;
    struct psf_menu_val *psf_m;
    int ncorner;
    
    long *istack_smp_arrow;
    long nnod_viz;
    long *num_patch;
} args_pthread_PSF_Arrow;



static void * add_num_psf_arrows_1thread(void *args){
    args_pthread_PSF_Arrow * p = (args_pthread_PSF_Arrow *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct psf_data     *psf_s = p->psf_s;
    struct psf_menu_val *psf_m = p->psf_m;
    int ncorner = p->ncorner;
    long nnod_viz = p->nnod_viz;
    
    long *num_patch =  p->num_patch;
    
    long lo = nnod_viz * id /     nthreads;
    long hi = nnod_viz * (id+1) / nthreads;
    num_patch[id] = add_num_psf_arrows(0, lo, hi, ncorner, psf_s, psf_m);
    return 0;
}

static void *  set_psf_arrows_to_buf_1thread(void *args){
    args_pthread_PSF_Arrow * p = (args_pthread_PSF_Arrow *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct psf_data     *psf_s = p->psf_s;
    struct psf_menu_val *psf_m = p->psf_m;
    int ncorner = p->ncorner;
    
    long *istack_smp_arrow =  p->istack_smp_arrow;
    long nnod_viz = p->nnod_viz;
    long *num_patch =  p->num_patch;
    
    long lo = nnod_viz * id /     nthreads;
    long hi = nnod_viz * (id+1) / nthreads;
    
    num_patch[id] = set_psf_arrows_to_buf(istack_smp_arrow[id], lo, hi, 
                                          ncorner, psf_s, psf_m, strided_buf);
    return 0;
}



static long add_num_psf_arrows_pthread(long ist_patch, const int nthreads,
                                       long *istack_arrow, int ncorner, 
                                       struct psf_data *psf_s, struct psf_menu_val *psf_m){
/* Allocate thread arguments. */
    args_pthread_PSF_Arrow *args
                = (args_pthread_PSF_Arrow *) malloc (nthreads * sizeof(args_pthread_PSF_Arrow));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Arrow.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
            
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].psf_s = psf_s;
        args[ip].psf_m = psf_m;
        args[ip].ncorner = ncorner;

        args[ip].nnod_viz = psf_s->nnod_viz;
        args[ip].num_patch = num_each;
                
        pthread_create(&thread_handles[ip], NULL, add_num_psf_arrows_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
 
    istack_arrow[0] = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        istack_arrow[ip+1] = istack_arrow[ip] + num_each[ip];
    }
    long inum_buf = istack_arrow[nthreads];
    free(num_each);
    free(thread_handles);
    free(args);
    return inum_buf;
}

static long set_psf_arrows_to_buf_pthread(long ist_patch, const int nthreads, 
                                          long *istack_smp_arrow, int ncorner, 
                                          struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                          struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Arrow *args
                = (args_pthread_PSF_Arrow *) malloc (nthreads * sizeof(args_pthread_PSF_Arrow));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Arrow.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = strided_buf;
        args[ip].psf_s = psf_s;
        args[ip].psf_m = psf_m;
        args[ip].ncorner = ncorner;

        args[ip].istack_smp_arrow = istack_smp_arrow;
        args[ip].nnod_viz = psf_s->nnod_viz;
        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, set_psf_arrows_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_each[ip];
    }
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}


long sel_add_num_psf_arrows_pthread(long ist_patch, const int nthreads,
                                    long *istack_arrow, int ncorner, 
                                    struct psf_data *psf_s, struct psf_menu_val *psf_m){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = add_num_psf_arrows_pthread(num_patch,
                                               nthreads, istack_arrow,
                                               ncorner, psf_s, psf_m);
    }else{
        num_patch = add_num_psf_arrows(num_patch, 0, psf_s->nnod_viz,
                                       ncorner, psf_s, psf_m);
        istack_arrow[1] = num_patch;
    };
    return num_patch;
}

long sel_psf_arrows_to_buf_pthread(long ist_patch, const int nthreads, 
                                   long *istack_smp_arrow, int ncorner, 
                                   struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                   struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_psf_arrows_to_buf_pthread(num_patch, nthreads,
                                                  istack_smp_arrow, ncorner,
                                                  psf_s, psf_m, strided_buf);
    }else{
        num_patch = set_psf_arrows_to_buf(num_patch, 0, psf_s->nnod_viz, ncorner,
                                          psf_s, psf_m, strided_buf);
    }
    return num_patch;
}
