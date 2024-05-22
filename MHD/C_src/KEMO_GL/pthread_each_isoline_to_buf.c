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
    
    struct gl_strided_buffer *strided_buf;
    struct psf_data          *psf_s;
    struct isoline_line_work *wk_iso_line;
    
    long icomp;
    double width;
    double v_line;
    double *f_color;

    long ist_patch;
    long ist;
    long ied;
    long *num_line;
} args_pthread_PSF_Isoline;


static void * count_isoline_npatch_1thread(void *args)
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
    
    num_line[id] = count_each_isoline_npatch(lo, hi, v_line, icomp, psf_s);
    return 0;
}

static void * set_each_isoline_to_list_1thread(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct isoline_line_work *wk_iso_line = p->wk_iso_line;
    struct psf_data *psf_s = p->psf_s;
    
    long icomp =      p->icomp;
    double v_line =   p->v_line;
    
    long ist_patch =  p->ist_patch;
    long *num_line =  p->num_line;
    
    long lo = psf_s->nele_viz * id /     nthreads;
    long hi = psf_s->nele_viz * (id+1) / nthreads;
    
    num_line[id] = set_each_isoline_to_list(ist_patch, lo, hi, v_line, icomp,
                                            psf_s, wk_iso_line);
    return 0;
}

static void * set_each_map_isoline_to_list_1thread(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct isoline_line_work *wk_iso_line = p->wk_iso_line;
    struct psf_data *psf_s = p->psf_s;
    
    long icomp =      p->icomp;
    double v_line =   p->v_line;
    
    long ist_patch =  p->ist_patch;
    long *num_line =  p->num_line;
    
    long lo = psf_s->nele_viz * id /     nthreads;
    long hi = psf_s->nele_viz * (id+1) / nthreads;
    
    num_line[id] = set_each_map_isoline_to_list(ist_patch, lo, hi, v_line, icomp,
                                                psf_s, wk_iso_line);
    return 0;
}

static void * set_each_isotube_to_buf_1thread(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct psf_data          *psf_s = p->psf_s;
    struct isoline_line_work *wk_iso_line = p->wk_iso_line;
    
    long ist = p->ist_patch;
    long lo = p->ist;
    long hi = p->ied;
    long *num_line =  p->num_line;
    
    num_line[id] = set_each_isotube_to_buf(ist, lo, hi, psf_s,
                                           wk_iso_line, strided_buf);
    return 0;
}

static void * set_each_isoline_to_buf_1thread(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct psf_data          *psf_s = p->psf_s;
    struct isoline_line_work *wk_iso_line = p->wk_iso_line;
    
    long ist = p->ist_patch;
    long lo = p->ist;
    long hi = p->ied;
    long *num_line =  p->num_line;
    
    num_line[id] = set_each_isoline_to_buf(ist, lo, hi, psf_s,
                                           wk_iso_line, strided_buf);
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
        
        pthread_create(&thread_handles[ip], NULL, count_isoline_npatch_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    
    istack_threads[0] = ist_patch;
    for(ip=0;ip<nthreads;ip++){
        istack_threads[ip+1] = istack_threads[ip] + num_line[ip];
    }
    free(num_line);
    free(thread_handles);
    free(args);
    return istack_threads[nthreads];
};

static long set_each_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                             double v_line, long icomp,
                                             struct psf_data *psf_s,
                                             struct isoline_line_work *wk_iso_line){
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
        
        args[ip].wk_iso_line = wk_iso_line;
        args[ip].psf_s = psf_s;

        args[ip].icomp = icomp;
        args[ip].v_line = v_line;
        
        args[ip].ist_patch = (istack_threads[ip] - istack_threads[0]);
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_isoline_to_list_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_line[nthreads-1];
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};

static long set_each_map_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                                 double v_line, long icomp,
                                                 struct psf_data *psf_s,
                                                 struct isoline_line_work *wk_iso_line){
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
        
        args[ip].wk_iso_line = wk_iso_line;
        args[ip].psf_s = psf_s;

        args[ip].icomp = icomp;
        args[ip].v_line = v_line;
        
        args[ip].ist_patch = (istack_threads[ip] - istack_threads[0]);
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_map_isoline_to_list_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_line[nthreads-1];
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};

static long set_each_isotube_to_buf_pthread(const long ist_patch, long ntot_line,
                                            const int nthreads, long *istack_threads,
                                            struct psf_data *psf_s,
                                            struct isoline_line_work *wk_iso_line,
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
        args[ip].wk_iso_line = wk_iso_line;

        args[ip].ist_patch = istack_threads[ip];
        args[ip].ist = (istack_threads[ip  ] - istack_threads[0]);
        args[ip].ied = (istack_threads[ip+1] - istack_threads[0]);
        
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_isotube_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_line[nthreads-1];
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};

static long set_each_isoline_to_buf_pthread(const long ist_patch, long ntot_line,
                                            const int nthreads, long *istack_threads,
                                            struct psf_data *psf_s,
                                            struct isoline_line_work *wk_iso_line,
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
        args[ip].wk_iso_line = wk_iso_line;

        args[ip].ist_patch = istack_threads[ip];
        args[ip].ist = (istack_threads[ip  ] - istack_threads[0]);
        args[ip].ied = (istack_threads[ip+1] - istack_threads[0]);
        
        args[ip].num_line = num_line;
        
        pthread_create(&thread_handles[ip], NULL, set_each_isoline_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_line[nthreads-1];
    free(num_line);
    free(thread_handles);
    free(args);
    return num_patch;
};



long sel_add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
                                         double v_line, long icomp, struct psf_data *psf_s,
                                         long *istack_threads){
    long num_patch;
    if(nthreads >1){
        num_patch = add_each_isoline_npatch_pthread(ist_patch, nthreads,
                                                    v_line, icomp, psf_s, istack_threads);
    }else{
        num_patch = ist_patch + count_each_isoline_npatch(IZERO, psf_s->nele_viz,
                                                          v_line, icomp, psf_s);
        istack_threads[1] = num_patch;
    }
    return num_patch;
};

long sel_each_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                      double v_line, long icomp,
                                      struct psf_data *psf_s,
                                      struct isoline_line_work *wk_iso_line){
    long num_patch = 0;
    if(nthreads > 1){
        num_patch = set_each_isoline_to_list_pthread(nthreads, istack_threads,
                                                     v_line, icomp, psf_s, wk_iso_line);
    }else{
        num_patch = set_each_isoline_to_list(IZERO, IZERO, psf_s->nele_viz,
                                             v_line, icomp, psf_s, wk_iso_line);
    }
    return num_patch;
};


long sel_each_map_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                          double v_line, long icomp,
                                          struct psf_data *psf_s,
                                          struct isoline_line_work *wk_iso_line){
    long num_patch = 0;
    if(nthreads > 1){
        num_patch = set_each_map_isoline_to_list_pthread(nthreads, istack_threads,
                                                     v_line, icomp, psf_s, wk_iso_line);
    }else{
        num_patch = set_each_map_isoline_to_list(IZERO, IZERO, psf_s->nele_viz,
                                                 v_line, icomp, psf_s, wk_iso_line);
    }
    return num_patch;
};

long sel_each_isotube_to_buf_pthread(const long ist_patch, long ntot_line,
                                     const int nthreads, long *istack_threads,
                                     struct psf_data *psf_s,
                                     struct isoline_line_work *wk_iso_line,
                                     struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_each_isotube_to_buf_pthread(num_patch, ntot_line,nthreads, istack_threads,
                                                    psf_s, wk_iso_line, strided_buf);
    }else{
        num_patch = set_each_isotube_to_buf(num_patch, IZERO, ntot_line,
                                            psf_s, wk_iso_line, strided_buf);
    }
    return num_patch;
};

long sel_each_isoline_to_buf_pthread(const long ist_patch, long ntot_line,
                                     const int nthreads, long *istack_threads,
                                     struct psf_data *psf_s,
                                     struct isoline_line_work *wk_iso_line,
                                     struct gl_strided_buffer *strided_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = set_each_isoline_to_buf_pthread(num_patch, ntot_line,
                                                    nthreads, istack_threads,
                                                    psf_s, wk_iso_line, strided_buf);
    }else{
        num_patch = set_each_isoline_to_buf(num_patch, IZERO, ntot_line,
                                            psf_s, wk_iso_line, strided_buf);
    }
    return num_patch;
};
