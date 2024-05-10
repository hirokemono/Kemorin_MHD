
/* set_fieldline_to_buf.c */

#include "set_fieldline_to_buf.h"
#include <pthread.h>

long count_fieldtubes_to_buf(int ncorner, struct psf_data *fline_s){
    long num_patch = 2 * fline_s->nele_viz * ncorner; 
	return num_patch;
};
long count_fieldlines_to_buf(struct psf_data *fline_s){
	return fline_s->nele_viz;
}

long set_fieldtubes_to_buf(long ist_patch, long ist_line, long ied_line,
                           int ncorner, struct psf_data *fline_s, 
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf){
    long inum_patch, k;
	int num_wall;
	int nd;
    long iele, inod;
	double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
	double x_line[6], dir_line[6], color_line[8];
	double norm_line[6];
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	inum_patch = ist_patch;
	for (iele=ist_line; iele<ied_line; iele++) {
		for (k = 0; k < 2; k++) {
			inod = fline_s->ie_viz[iele][k] - 1;
			for (nd=0; nd<3; nd++) {
				x_line[3*k+nd] =   (float) fline_s->xyzw_viz[inod*IFOUR + nd];
				dir_line[3*k+nd] = (float) fline_s->dir_nod[inod*IFOUR + nd];
			};
			for (nd=0; nd<4; nd++) {color_line[4*k+nd] = (float) fline_s->color_nod[4*inod+nd];};
		};
		find_normal_of_line(norm_line, x_line, dir_line);
		num_wall = set_tube_vertex(ncorner, fline_m->fieldline_thick, 
								   x_line, dir_line, norm_line, color_line,
                                   xyzw, norm, col);
		
		for (k=0; k<3*num_wall; k++) {
            set_node_stride_buffer((ITHREE*inum_patch+k), strided_buf, point_buf);
            for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf->igl_xyzw] = xyzw[4*k+nd];
                strided_buf->v_buf[nd+point_buf->igl_norm] = norm[4*k+nd];
                strided_buf->v_buf[nd+point_buf->igl_color] = col[4*k+nd];
            };
		};
		inum_patch = inum_patch + num_wall; 
	};
	return inum_patch;
};


typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;
    struct gl_local_buffer_address  *point_buf;

    int ncorner;
    struct psf_data       *fline_s;
    struct fline_menu_val *fline_m;
    
    long *num_patch;
} args_pthread_fieldline;




static void * set_fieldtubes_to_buf_1thread(void *args){
    args_pthread_fieldline * p = (args_pthread_fieldline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct gl_local_buffer_address *point_buf = p->point_buf;
    
    int ncorner = p->ncorner;
    struct psf_data       *fline_s = p->fline_s;
    struct fline_menu_val *fline_m = p->fline_m;
    
    long *num_patch =  p->num_patch;
    
    long lo = fline_s->nele_viz * id /     nthreads;
    long hi = fline_s->nele_viz * (id+1) / nthreads;
    long ist_patch = 2 * ncorner * lo;
        
    num_patch[id] = set_fieldtubes_to_buf(ist_patch, lo, hi, ncorner, fline_s, fline_m,
                                          strided_buf, point_buf);
    return 0;
}


long set_fieldtubes_to_buf_pthread(long ist_patch, const int nthreads, 
                                   int ncorner, struct psf_data *fline_s, 
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf,
                                   struct gl_local_buffer_address **para_point_buf){
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
        args[ip].point_buf = para_point_buf[ip];
        
        args[ip].fline_s = fline_s;
        args[ip].fline_m = fline_m;
        args[ip].ncorner = ncorner;

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



long set_fieldlines_to_buf(long ist_patch, long ist_line, long ied_line,
                           struct psf_data *fline_s,
                           struct fline_menu_val *fline_m,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf){
	long iele, k, nd, inod;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	for(iele=ist_line; iele<ied_line; iele++){
		for(k=0;k<ITWO;k++){
			inod =fline_s->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITWO*iele+k), strided_buf, point_buf);
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf->igl_xyzw] 
                    = fline_s->xyzw_viz[4*inod + nd];
            };
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf->igl_color]
                    = fline_s->color_nod[4*inod + nd];
            };
		};
	};
	
	return fline_s->nele_viz;
}

static void * set_fieldlines_to_buf_1thread(void *args){
    args_pthread_fieldline * p = (args_pthread_fieldline *) args;
    int id =       p->id;
    int nthreads = p->nthreads;
    
    struct gl_strided_buffer *strided_buf = p->strided_buf;
    struct gl_local_buffer_address *point_buf = p->point_buf;
    
    struct psf_data       *fline_s = p->fline_s;
    struct fline_menu_val *fline_m = p->fline_m;
    
    long *num_patch =  p->num_patch;
    
    long lo = fline_s->nele_viz * id /     nthreads;
    long hi = fline_s->nele_viz * (id+1) / nthreads;
        
    num_patch[id] = set_fieldlines_to_buf(lo, lo, hi, fline_s, fline_m,
                                          strided_buf, point_buf);
    return 0;
}

long set_fieldlines_to_buf_pthread(long ist_patch, const int nthreads, 
                                   struct psf_data *fline_s, 
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf,
                                   struct gl_local_buffer_address **para_point_buf){
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
        args[ip].point_buf = para_point_buf[ip];
        
        args[ip].fline_s = fline_s;
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

