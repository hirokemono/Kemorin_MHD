/*
 *  pthread_each_isoline_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include <pthread.h>

#include "pthread_each_isoline_to_buf.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;
    struct psf_data *psf_s;

    long icomp;
    double width;
    double v_line;
    double *f_color;

    long ist_patch;
    long *num_line;
} args_pthread_PSF_Isoline;


static void * add_isoline_npatch_each(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct psf_data *psf_s = p->psf_s;
    long icomp = p->icomp;
    double v_line = p->v_line;
    long *num_line = p->num_line;
    
    long lo = psf_s->nele_viz * id /     nthreads;
    long hi = psf_s->nele_viz * (id+1) / nthreads;
    
    num_line[id] = add_each_isoline_npatch(IZERO, lo, hi, v_line, icomp, psf_s);
    return 0;
}

static void * set_each_isoline_to_buf_each(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct psf_data *psf_s = p->psf_s;
    
    long icomp =      p->icomp;
    double width =    p->width;
    double v_line =   p->v_line;
    double *f_color = p->f_color;
    
    long ist_patch =  p->ist_patch;
    long *num_line =  p->num_line;
    
    long lo = psf_s->nele_viz * id /     nthreads;
    long hi = psf_s->nele_viz * (id+1) / nthreads;
    
    num_line[id] = set_each_isoline_to_buf(ist_patch, lo, hi,
                                           width, v_line, icomp, f_color,
                                           psf_s, strided_buf);
    return 0;
}

static void * set_each_map_isoline_to_buf_each(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct psf_data *psf_s = p->psf_s;
    
    long icomp =      p->icomp;
    double width =    p->width;
    double v_line =   p->v_line;
    double *f_color = p->f_color;
    
    long ist_patch =  p->ist_patch;
    long *num_line =  p->num_line;
    
    long lo = psf_s->nele_viz * id /     nthreads;
    long hi = psf_s->nele_viz * (id+1) / nthreads;
    
    num_line[id] =  set_each_map_isoline_to_buf(ist_patch, lo, hi,
                                                width, v_line, icomp, f_color,
                                                psf_s, strided_buf);
    return 0;
}

static long add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
                                            double v_line, long icomp, struct psf_data *psf_s,
                                            long *istack_threads){
/* Allocate thread arguments. */
    args_pthread_PSF_Isoline *args
            = (args_pthread_PSF_Isoline *) malloc (nthreads * sizeof(args_pthread_PSF_Isoline));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Isoline.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    int ip;
    long *num_line = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;
        
        args[ip].icomp = icomp;
        args[ip].v_line = v_line;
        args[ip].psf_s = psf_s;
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, add_isoline_npatch_each, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    
    istack_threads[0] = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        istack_threads[ip+1] = istack_threads[ip] + num_line[ip];
    }
    free(num_line);
    free(thread_handles);
    free(args);
//    printf("Parallel count %ld\n", inum_patch);
    return istack_threads[nthreads];
};

static long set_each_isoline_to_buf_pthread(const long ist_patch,
                                            const int nthreads, long *istack_threads,
                                            double width, double v_line,
                                            long icomp, double *f_color,
                                            struct psf_data *psf_s,
                                            struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Isoline *args
            = (args_pthread_PSF_Isoline *) malloc (nthreads * sizeof(args_pthread_PSF_Isoline));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Isoline.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    int ip;
    long *num_line = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;
        
        args[ip].strided_buf = strided_buf;
        args[ip].psf_s = psf_s;

        args[ip].icomp = icomp;
        args[ip].width = width;
        args[ip].v_line = v_line;
        args[ip].f_color = f_color;
        
        args[ip].ist_patch = istack_threads[ip];
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_isoline_to_buf_each, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_line[ip];
    }
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};

static long set_each_map_isoline_to_buf_pthread(const long ist_patch,
                                                const int nthreads, long *istack_threads,
                                                double width, double v_line,
                                                long icomp, double *f_color,
                                                struct psf_data *psf_s,
                                                struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Isoline *args
            = (args_pthread_PSF_Isoline *) malloc (nthreads * sizeof(args_pthread_PSF_Isoline));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Isoline.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    int ip;
    long *num_line = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;
        
        args[ip].strided_buf = strided_buf;
        args[ip].psf_s = psf_s;

        args[ip].icomp = icomp;
        args[ip].width = width;
        args[ip].v_line = v_line;
        args[ip].f_color = f_color;
        
        args[ip].ist_patch = istack_threads[ip];
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_map_isoline_to_buf_each, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_line[ip];
    }
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};



long sel_add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
                                         double v_line, long icomp, struct psf_data *psf_s,
                                         long *istack_threads){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = add_each_isoline_npatch_pthread(num_patch, nthreads,
                                                    v_line, icomp, psf_s, istack_threads);
    }else{
        num_patch = add_each_isoline_npatch(num_patch, IZERO, psf_s->nele_viz,
                                            v_line, icomp, psf_s);
    }
    return num_patch;
};

long sel_each_isoline_to_buf_pthread(const long ist_patch,
                                     const int nthreads, long *istack_threads,
                                     double width, double v_line,
                                     long icomp, double *f_color,
                                     struct psf_data *psf_s,
                                     struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_each_isoline_to_buf_pthread(num_patch, nthreads, istack_threads,
                                                        width, v_line, icomp, f_color,
                                                        psf_s, strided_buf);
    }else{
        num_patch = set_each_isoline_to_buf(num_patch, IZERO, psf_s->nele_viz,
                                                width, v_line, icomp, f_color,
                                                psf_s, strided_buf);
    }
    return num_patch;
};

long sel_each_map_isoline_to_buf_pthread(const long ist_patch,
                                         const int nthreads, long *istack_threads,
                                         double width, double v_line,
                                         long icomp, double *f_color,
                                         struct psf_data *psf_s,
                                         struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_each_map_isoline_to_buf_pthread(num_patch, nthreads, istack_threads,
                                                        width, v_line, icomp, f_color,
                                                        psf_s, strided_buf);
    }else{
        num_patch = set_each_map_isoline_to_buf(num_patch, IZERO, psf_s->nele_viz,
                                                width, v_line, icomp, f_color,
                                                psf_s, strided_buf);
    }
    return num_patch;
};
