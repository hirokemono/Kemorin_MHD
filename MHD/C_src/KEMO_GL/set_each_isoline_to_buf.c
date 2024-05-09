/*
 *  set_each_isoline_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "set_each_isoline_to_buf.h"
#include <pthread.h>

static void copy_each_triangle_postion(long ntot_comp, long ie_viz[3],
                                       double *xyzw_viz, double *d_nod, long icomp,
									   double xyz_tri[9], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
		xyz_tri[3*k  ] = xyzw_viz[inod*IFOUR + 0];
		xyz_tri[3*k+1] = xyzw_viz[inod*IFOUR + 1];
		xyz_tri[3*k+2] = xyzw_viz[inod*IFOUR + 2];
	};
	return;
};
static void copy_each_triangle_postion_norm(long ntot_comp, long ie_viz[3], double *xyzw_viz, double *norm_nod,
											double *d_nod, long icomp,
											double xyz_tri[9], double nrm_tri[9], double d_tri[3]){
	int k;
	long inod;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
		xyz_tri[3*k  ] = xyzw_viz[inod*IFOUR + 0];
		xyz_tri[3*k+1] = xyzw_viz[inod*IFOUR + 1];
		xyz_tri[3*k+2] = xyzw_viz[inod*IFOUR + 2];
		nrm_tri[3*k  ] = norm_nod[4*inod+0];
		nrm_tri[3*k+1] = norm_nod[4*inod+1];
		nrm_tri[3*k+2] = norm_nod[4*inod+2];
	};
	return;
};

static void copy_each_triangle_map_postion(long ntot_comp, long ie_viz[3], double *xyzw_viz,
                                           double *d_nod, long icomp,
                                           double xyz_map[9], double nrm_tri[9], double d_tri[3]){
	double xyz_tri[9];
	long inod;
	int k;
	for (k = 0; k < 3; k++) {
		inod = ie_viz[k] - 1;
		
		d_tri[k] =       d_nod[inod*ntot_comp + icomp];
		xyz_tri[3*k  ] = xyzw_viz[inod*IFOUR + 0];
		xyz_tri[3*k+1] = xyzw_viz[inod*IFOUR + 1];
		xyz_tri[3*k+2] = xyzw_viz[inod*IFOUR + 2];
		nrm_tri[3*k  ] = 0.0;
		nrm_tri[3*k+1] = 0.0;
		nrm_tri[3*k+2] = 1.0;
	};
	projection_patch_4_map(xyz_tri, xyz_map);
	return;
};

long add_each_isoline_npatch(const long ist_patch, const long ist, const long ied,
                             const double v_line, long icomp, struct psf_data *psf_s){
    double d_tri[3], xyz_tri[9];
    long iele;
    int idraw;
    
    long inum_patch = ist_patch;
    for(iele=ist;iele<ied;iele++){
        copy_each_triangle_postion(psf_s->ncomptot,
                                   &psf_s->ie_viz[iele][0], psf_s->xyzw_viz,
                                   psf_s->d_nod, icomp, xyz_tri, d_tri);
        /*  find isoline */
        idraw = find_isoline_on_triangle(xyz_tri, d_tri, v_line);
        /*  count isoline */
        inum_patch = inum_patch + 12 * idraw;
    };
    return inum_patch;
};

long set_each_isoline_to_buf(const long ist_patch,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                             struct gl_strided_buffer *strided_buf,
                             struct gl_local_buffer_address *point_buf){
	double d_tri[3], xyz_tri[9], nrm_tri[9];
	double x_line[6], dir_line[6], norm_line[6], color_line[8];
	int hex_tube[12][3];
	
	int idraw, nd;
	long iele;
	
	long inum_patch = ist_patch;
	copy_hex_tube_pp(hex_tube);
	for (iele = ist; iele < ied; iele++) {
		copy_each_triangle_postion_norm(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                        psf_s->xyzw_viz, psf_s->norm_nod,
										psf_s->d_nod, icomp, xyz_tri, nrm_tri, d_tri);
		/*  find isoline */
		idraw = set_isoline_on_triangle(x_line, dir_line, norm_line, 
										xyz_tri, nrm_tri, d_tri, v_line);
		/* draw isoline */
		if(idraw == 1){
			for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
			for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
			inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line, 
												 x_line, dir_line, norm_line,
                                                 strided_buf, point_buf);
		};
	};

	return inum_patch;
};

long set_each_map_isoline_to_buf(const long ist_patch,
                                 const long ist, const long ied,
                                 double width, double v_line,
                                 long icomp, double *f_color,
                                 struct psf_data *psf_s,
                                 struct gl_strided_buffer *strided_buf,
                                 struct gl_local_buffer_address *point_buf){
    double d_tri[3], nrm_tri[9];
    double xyz_map[9];
    double x_line[6], dir_line[6], norm_line[6], color_line[8];
    int hex_tube[12][3];
    
    int idraw, nd;
    long iele;
    
    long inum_patch = ist_patch;
    copy_hex_tube_pp(hex_tube);
    for (iele = ist; iele < ied; iele++) {
        copy_each_triangle_map_postion(psf_s->ncomptot, &psf_s->ie_viz[iele][0],
                                       psf_s->xyzw_viz, psf_s->d_nod, icomp,
                                       xyz_map, nrm_tri, d_tri);
        idraw = set_isoline_on_triangle(x_line, dir_line, norm_line,
                                        xyz_map, nrm_tri, d_tri, v_line);
        /*  draw isoline */
        if(idraw == 1){
            for(nd=0;nd<4;nd++){color_line[  nd] = f_color[nd];};
            for(nd=0;nd<4;nd++){color_line[4+nd] = f_color[nd];};
            inum_patch = append_line_tube_to_buf(inum_patch, hex_tube, width, color_line,
                                                 x_line, dir_line, norm_line,
                                                 strided_buf, point_buf);
        };
    };
    return inum_patch;
};



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
    struct gl_local_buffer_address *point_buf = p->point_buf;
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
                                           psf_s, strided_buf, point_buf);
    return 0;
}

static void * set_each_map_isoline_to_buf_each(void *args)
{
    args_pthread_PSF_Isoline * p = (args_pthread_PSF_Isoline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct gl_local_buffer_address *point_buf = p->point_buf;
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
                                                psf_s, strided_buf, point_buf);
    return 0;
}

long add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
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

long set_each_isoline_to_buf_pthread(const long ist_patch,
                                     const int nthreads, long *istack_threads,
                                     double width, double v_line,
                                     long icomp, double *f_color,
                                     struct psf_data *psf_s,
                                     struct gl_strided_buffer *strided_buf,
                                     struct gl_local_buffer_address **para_point_buf){
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
        args[ip].point_buf = para_point_buf[ip];
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

long set_each_map_isoline_to_buf_pthread(const long ist_patch,
                                         const int nthreads, long *istack_threads,
                                         double width, double v_line,
                                         long icomp, double *f_color,
                                         struct psf_data *psf_s,
                                         struct gl_strided_buffer *strided_buf,
                                         struct gl_local_buffer_address **para_point_buf){
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
        args[ip].point_buf = para_point_buf[ip];
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
