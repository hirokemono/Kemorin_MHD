/*
 *  pthread_PSF_patches_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include <pthread.h>

#include "pthread_PSF_patches_to_buf.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct psf_data     **psf_s;
    struct kemo_array_control *psf_a;
    int i_psf;

    long ist_patch;
    long ist_psf;
    long ied_psf;
    long *num_patch;
} args_pthread_PSF_Patch;


static void * set_psf_nodes_to_buf_1thread(void *args)
{
    args_pthread_PSF_Patch * p = (args_pthread_PSF_Patch *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct psf_data     **psf_s = p->psf_s;
    int i_psf = p->i_psf;

    long ist_patch = p->ist_patch;
    long ist_psf = p->ist_psf;
    long ied_psf = p->ied_psf;
    long *num_patch =  p->num_patch;
    
    long lo = (ied_psf-ist_psf) * id /     nthreads;
    long hi = (ied_psf-ist_psf) * (id+1) / nthreads;
    num_patch[id] = set_psf_nodes_to_buf((lo+ist_patch), (lo+ist_psf), (hi-lo),
                                         psf_s[i_psf], strided_buf);
    return 0;
}

static long set_psf_nodes_to_buf_pthread(long ipatch_in, int nthreads, long ist_psf, long num,
                                         int i_psf, struct psf_data **psf_s,
                                         struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Patch *args
            = (args_pthread_PSF_Patch *) malloc (nthreads * sizeof(args_pthread_PSF_Patch));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Patch.\n"); exit(1);}
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
        args[ip].i_psf = i_psf;

        args[ip].ist_patch = ipatch_in;
        args[ip].ist_psf = ist_psf;
        args[ip].ied_psf = num;
        args[ip].num_patch = num_each;
        
        pthread_create(&thread_handles[ip], NULL, set_psf_nodes_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = ipatch_in;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_each[ip];
    }
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
};



long sel_psf_nodes_to_buf_pthread(long ipatch_in, const int nthreads,
                                  long ist_psf, long num,
                                  int i_psf, struct psf_data **psf_s,
                                  struct gl_strided_buffer *strided_buf){
    long num_patch = ipatch_in;
    if(nthreads > 1){
        num_patch = set_psf_nodes_to_buf_pthread(num_patch, nthreads, ist_psf, num,
                                                 i_psf, psf_s, strided_buf);
    }else{
        num_patch = set_psf_nodes_to_buf(num_patch, ist_psf, num,
                                         psf_s[i_psf], strided_buf);
    };
    return num_patch;
}





static void * set_psf_patches_to_buf_1thread(void *args)
{
    args_pthread_PSF_Patch * p = (args_pthread_PSF_Patch *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct psf_data     **psf_s =       p->psf_s;
    struct kemo_array_control *psf_a = p->psf_a;
       
    long ist_patch = p->ist_patch;
    long ist_psf = p->ist_psf;
    long ied_psf = p->ied_psf;
    long *num_patch =  p->num_patch;
    
    long lo = (ied_psf-ist_psf) * id /     nthreads;
    long hi = (ied_psf-ist_psf) * (id+1) / nthreads;
    num_patch[id] = set_psf_patches_to_buf((lo+ist_patch), (lo+ist_psf), (hi+ist_psf),
                                           psf_s, psf_a, strided_buf);
    return 0;
}

static long set_psf_patches_to_buf_pthread(long ipatch_in, int nthreads,
                                           long ist_psf, long ied_psf,
                                           struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                           struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Patch *args
            = (args_pthread_PSF_Patch *) malloc (nthreads * sizeof(args_pthread_PSF_Patch));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Patch.\n"); exit(1);}
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
        args[ip].psf_a = psf_a;

        args[ip].ist_patch = ipatch_in;
        args[ip].ist_psf = ist_psf;
        args[ip].ied_psf = ied_psf;
        args[ip].num_patch = num_each;
        
        pthread_create(&thread_handles[ip], NULL, set_psf_patches_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = ipatch_in;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_each[ip];
    }
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
};

long sel_psf_patches_to_buf_pthread(long ipatch_in, const int nthreads,
                                    long ist_psf, long ied_psf,
                                    struct psf_data **psf_s,
                                    struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *strided_buf){
    long num_patch = ipatch_in;
    if(nthreads > 1){
        num_patch = set_psf_patches_to_buf_pthread(num_patch, nthreads, ist_psf, ied_psf,
                                                   psf_s, psf_a, strided_buf);
    }else{
        num_patch = set_psf_patches_to_buf(num_patch, ist_psf, ied_psf,
                                           psf_s, psf_a, strided_buf);
    };
    return num_patch;
}



static void * set_psf_textures_to_buf_1thread(void *args)
{
    args_pthread_PSF_Patch * p = (args_pthread_PSF_Patch *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct psf_data     **psf_s =       p->psf_s;
    struct kemo_array_control *psf_a = p->psf_a;

    long ist_psf = p->ist_psf;
    long ied_psf = p->ied_psf;
    long *num_patch =  p->num_patch;

    long lo = (ied_psf-ist_psf) * id /     nthreads;
    long hi = (ied_psf-ist_psf) * (id+1) / nthreads;
    num_patch[id] = set_psf_textures_to_buf(lo, (lo+ist_psf), (hi+ist_psf), psf_s, psf_a,
                                            strided_buf);
    return 0;
}

static long set_psf_textures_to_buf_pthread(int nthreads,
                                            long ist_psf, long ied_psf,
                                            struct psf_data **psf_s,
                                            struct kemo_array_control *psf_a,
                                            struct gl_strided_buffer *strided_buf){
/* Allocate thread arguments. */
    args_pthread_PSF_Patch *args
            = (args_pthread_PSF_Patch *) malloc (nthreads * sizeof(args_pthread_PSF_Patch));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_PSF_Patch.\n"); exit(1);}
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
        args[ip].psf_a = psf_a;

        args[ip].ist_psf = ist_psf;
        args[ip].ied_psf = ied_psf;
        args[ip].num_patch = num_each;
        
        pthread_create(&thread_handles[ip], NULL, set_psf_textures_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = 0;
    for(ip=0;ip<nthreads;ip++){
        num_patch = num_patch + num_each[ip];
    }
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
};

long sel_psf_textures_to_buf_pthread(int nthreads, long ist_psf, long ied_psf,
                                     struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *strided_buf){
    long num_patch = 0;
    if(nthreads > 1){
        num_patch = set_psf_textures_to_buf_pthread(nthreads, ist_psf, ied_psf,
                                                    psf_s, psf_a, strided_buf);
    }else{
        num_patch = set_psf_textures_to_buf(0, ist_psf, ied_psf,
                                            psf_s, psf_a, strided_buf);
    };
    return num_patch;
}
