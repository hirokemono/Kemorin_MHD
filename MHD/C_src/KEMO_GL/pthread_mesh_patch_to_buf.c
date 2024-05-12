/*
 *  pthread_mesh_patch_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include <pthread.h>
#include "pthread_mesh_patch_to_buf.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct viewer_mesh *mesh_s;
    int shading_mode;
    int polygon_mode;
    
    long nsurf_viz;
    long *iele_patch;
    
    long *num_patch;
} args_pthread_mesh_patch;

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;

    struct viewer_mesh *mesh_s;
    int ist_grp;
    int ied_grp;
    int  *item_grp;
    double *f_color;
    double node_diam;
    
    long istack_patch;
    long *num_patch;
} args_pthread_mesh_node;


static void * add_mesh_patch_to_buffer_1thread(void *arg){
    args_pthread_mesh_patch * p = (args_pthread_mesh_patch *) arg;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct viewer_mesh *mesh_s = p->mesh_s;
    int shading_mode =  p->shading_mode;
    int polygon_mode =  p->polygon_mode;
    long nsurf_viz =  p->nsurf_viz;
    long *iele_patch = p->iele_patch;;

    long *num_patch =  p->num_patch;
    
    long lo = nsurf_viz * id /     nthreads;
    long hi = nsurf_viz * (id+1) / nthreads;
    long ist_patch = lo;

    num_patch[id] = set_mesh_patch_to_buffer(shading_mode, polygon_mode, mesh_s,
                                             ist_patch, lo, hi, iele_patch,
                                             strided_buf);
    return 0;
}

static void * set_each_mesh_grid_to_buf_1thread(void *arg){
    args_pthread_mesh_node * p = (args_pthread_mesh_node *) arg;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct viewer_mesh *mesh_s = p->mesh_s;
    int ist_grp =  p->ist_grp;
    int ied_grp =  p->ied_grp;
    int *item_grp =     p->item_grp;;
    double *f_color =   p->f_color;

    long istack_patch = p->istack_patch;
    long *num_patch =  p->num_patch;
    
    int lo = (ied_grp - ist_grp) * id /     nthreads;
    int hi = (ied_grp - ist_grp) * (id+1) / nthreads;
    long ist_edge = (mesh_s->nnod_4_edge-1) * lo + istack_patch;
        
    num_patch[id] = set_each_mesh_grid_to_buf((lo+ist_grp), (hi+ist_grp), item_grp, mesh_s,
                                              f_color, ist_edge, strided_buf);
    return 0;
}

static void * each_grp_nod_ico_to_buf_1thread(void *arg){
    args_pthread_mesh_node * p = (args_pthread_mesh_node *) arg;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    
    struct viewer_mesh *mesh_s = p->mesh_s;
    int ist_grp =  p->ist_grp;
    int ied_grp =  p->ied_grp;
    int *item_grp =     p->item_grp;;
    double *f_color =   p->f_color;
    double node_diam =  p->node_diam;

    long istack_patch = p->istack_patch;
    long *num_patch =  p->num_patch;
    
    long lo = (ied_grp - ist_grp) * id /     nthreads;
    long hi = (ied_grp - ist_grp) * (id+1) / nthreads;
    long ist_patch = num_icosahedron_patch() * lo + istack_patch;
        
    num_patch[id] = set_each_group_node_ico_to_buf(ist_patch,
                                                   (lo+ist_grp), (hi+ist_grp), item_grp,
                                                   mesh_s, node_diam, f_color,
                                                   strided_buf);

    return 0;
}

long set_mesh_patch_to_buffer_pthread(int shading_mode, int polygon_mode,
                                      struct viewer_mesh *mesh_s,
                                      int nthreads, long ist_tri,
                                      long ntot_patch, long *iele_patch,
                                      struct gl_strided_buffer *mesh_buf){
/* Allocate thread arguments. */
    args_pthread_mesh_patch *args
                    = (args_pthread_mesh_patch *) malloc (nthreads * sizeof(args_pthread_mesh_patch));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_mesh_patch.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = mesh_buf;
        
        args[ip].mesh_s =    mesh_s;
        args[ip].shading_mode =  shading_mode;
        args[ip].polygon_mode =  polygon_mode;

        args[ip].nsurf_viz =  ntot_patch;
        args[ip].iele_patch = iele_patch;

        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, add_mesh_patch_to_buffer_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}

long set_each_mesh_grid_to_buf_pthread(long ist_patch, const int nthreads,
                                       int ist_grp, int ied_grp, int *item_grp,
                                       struct viewer_mesh *mesh_s, double f_color[4],
                                       struct gl_strided_buffer *mesh_buf){
/* Allocate thread arguments. */
    args_pthread_mesh_node *args
                = (args_pthread_mesh_node *) malloc (nthreads * sizeof(args_pthread_mesh_node));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_mesh_node.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = mesh_buf;
        
        args[ip].mesh_s =    mesh_s;
        args[ip].ist_grp =  ist_grp;
        args[ip].ied_grp =  ied_grp;
        args[ip].item_grp =  item_grp;
        args[ip].f_color =   f_color;

        args[ip].istack_patch = ist_patch;
        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, set_each_mesh_grid_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}

long each_grp_nod_ico_to_buf_pthread(long ist_patch, const int nthreads,
                                     int ist_grp, int ied_grp, int *item_grp,
                                     struct viewer_mesh *mesh_s, double node_diam,
                                     double f_color[4],
                                     struct gl_strided_buffer *mesh_buf){
/* Allocate thread arguments. */
    args_pthread_mesh_node *args
                = (args_pthread_mesh_node *) malloc (nthreads * sizeof(args_pthread_mesh_node));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread_mesh_node.\n"); exit(1);}
/* Initialize thread handles and barrier. */
    pthread_t* thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
        
    int ip;
    long *num_each = (long *) malloc (nthreads * sizeof(long));
    for(ip=0;ip<nthreads;ip++) {
        args[ip].id = ip;
        args[ip].nthreads = nthreads;

        args[ip].strided_buf = mesh_buf;
        
        args[ip].mesh_s =    mesh_s;
        args[ip].ist_grp =  ist_grp;
        args[ip].ied_grp =  ied_grp;
        args[ip].item_grp =  item_grp;
        args[ip].f_color =   f_color;
        args[ip].node_diam = node_diam;

        args[ip].istack_patch = ist_patch;
        args[ip].num_patch = num_each;
            
        pthread_create(&thread_handles[ip], NULL, each_grp_nod_ico_to_buf_1thread, &args[ip]);
    }
    for(ip=0;ip<nthreads;ip++){pthread_join(thread_handles[ip], NULL);}
    long num_patch = num_each[ip];
    free(num_each);
    free(thread_handles);
    free(args);
    return num_patch;
}

long sel_mesh_patch_to_buffer_pthread(int shading_mode, int polygon_mode,
                                      struct viewer_mesh *mesh_s,
                                      int nthreads, long ist_tri,
                                      long ntot_patch, long *iele_patch,
                                      struct gl_strided_buffer *mesh_buf){
    long num_patch = 0;
    if(nthreads > 1){
        num_patch = set_mesh_patch_to_buffer_pthread(shading_mode, polygon_mode,
                                                     mesh_s, nthreads, num_patch,
                                                     ntot_patch, iele_patch,
                                                     mesh_buf);
    }else{
        num_patch = set_mesh_patch_to_buffer(shading_mode, polygon_mode, mesh_s,
                                             num_patch, IZERO, ntot_patch, iele_patch,
                                             mesh_buf);
    };
    return num_patch;
}

long sel_each_mesh_grid_to_buf_pthread(long ist_patch, const int nthreads,
                                       int ist_grp, int ied_grp, int *item_grp,
                                       struct viewer_mesh *mesh_s, double f_color[4],
                                       struct gl_strided_buffer *mesh_buf){
    long num_edge = ist_patch;
    if(nthreads > 0){
        num_edge = set_each_mesh_grid_to_buf_pthread(num_edge, nthreads,
                                                     ist_grp, ied_grp, item_grp,
                                                     mesh_s, f_color, mesh_buf);
    }else{
        num_edge = set_each_mesh_grid_to_buf(ist_grp, ied_grp, item_grp,
                                             mesh_s, f_color, num_edge, mesh_buf);
    }
    return num_edge;
}

long sel_each_grp_nod_ico_to_buf_pthread(long ist_patch, const int nthreads,
                                         int ist_grp, int ied_grp, int *item_grp,
                                         struct viewer_mesh *mesh_s, double node_diam,
                                         double f_color[4],
                                         struct gl_strided_buffer *mesh_buf){
    long num_patch = ist_patch;
    if(nthreads > 1){
        num_patch = each_grp_nod_ico_to_buf_pthread(ist_patch, nthreads, ist_grp, ied_grp,
                                                     item_grp, mesh_s, node_diam, f_color,
                                                     mesh_buf);
      }else{
        num_patch = set_each_group_node_ico_to_buf(ist_patch, ist_grp, ied_grp,
                                                   item_grp, mesh_s, node_diam, f_color,
                                                   mesh_buf);
      };
    return num_patch;
}

