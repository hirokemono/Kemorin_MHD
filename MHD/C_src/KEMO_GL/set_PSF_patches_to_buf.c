
/* set_PSF_patches_to_buf.c */

#include <pthread.h>
#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

static float arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

long count_psf_nodes_to_buf(long ist_psf, long ied_psf){
	return (ied_psf - ist_psf);
};

long set_psf_nodes_to_buf(long ipatch_in, long ist_psf, long ied_psf, int shading_mode, 
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf,
                          struct gl_local_buffer_address *point_buf){
    long ipatch = ipatch_in;
    long lk, inum, iele, inod;
    int nd, ipsf;
	
	for(inum=ist_psf;inum<ied_psf;inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
		for (lk = 0; lk < ITHREE; lk++) {
			inod = psf_s[ipsf]->ie_viz[iele][lk] - 1;
			
            set_node_stride_buffer((ITHREE*ipatch+lk), strided_buf, point_buf);
			for(nd=0;nd<3;nd++){
                strided_buf->v_buf[nd+point_buf->igl_xyzw] = psf_s[ipsf]->xyzw_viz[IFOUR*inod + nd];
            };
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf->igl_color] = psf_s[ipsf]->color_nod[IFOUR*inod+nd];
            };
			if (shading_mode == SMOOTH_SHADE){
                for(nd=0;nd<3;nd++){
                    strided_buf->v_buf[nd+point_buf->igl_norm] = psf_s[ipsf]->norm_nod[IFOUR*inod+nd];
                };
			} else {
				for(nd=0;nd<3;nd++){
                    strided_buf->v_buf[nd+point_buf->igl_norm] = psf_s[ipsf]->norm_ele[IFOUR*iele+nd];
                };
			};
			if(psf_m[ipsf]->polygon_mode_psf == REVERSE_POLYGON){
				for(nd=0;nd<3;nd++){strided_buf->v_buf[nd+point_buf->igl_norm] = -strided_buf->n_draw[nd];};
			};
		};
        ipatch = ipatch + 1;
    };
    return ipatch;
}

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;
    struct gl_local_buffer_address  *point_buf;

    struct psf_data     **psf_s;
    struct psf_menu_val **psf_m;
    struct kemo_array_control *psf_a;
    int shading_mode;
    
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
    struct gl_local_buffer_address *point_buf = p->point_buf;
    
    struct psf_data     **psf_s =       p->psf_s;
    struct psf_menu_val **psf_m =       p->psf_m;
    struct kemo_array_control *psf_a = p->psf_a;
   
    int shading_mode = p->shading_mode;
    
    long ist_psf = p->ist_psf;
    long ied_psf = p->ied_psf;
    long *num_patch =  p->num_patch;
    
    long lo = ist_psf + (ied_psf-ist_psf) * id /     nthreads;
    long hi = ist_psf + (ied_psf-ist_psf) * (id+1) / nthreads;
    num_patch[id] = set_psf_nodes_to_buf(lo, lo, hi, shading_mode, 
                                         psf_s, psf_m, psf_a,
                                         strided_buf, point_buf);
    return 0;
}

long set_psf_nodes_to_buf_pthread(long ipatch_in, int nthreads,
                                  long ist_psf, long ied_psf, int shading_mode, 
                                  struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                  struct kemo_array_control *psf_a,
                                  struct gl_strided_buffer *strided_buf,
                                  struct gl_local_buffer_address **para_point_buf){
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
        args[ip].point_buf = para_point_buf[ip];
        args[ip].psf_s = psf_s;
        args[ip].psf_m = psf_m;
        args[ip].psf_a = psf_a;
        args[ip].shading_mode = shading_mode;

        args[ip].ist_psf = ist_psf;
        args[ip].ied_psf = ied_psf;
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




void set_psf_textures_to_buf(long ist_psf, long ied_psf,
                             struct psf_data **psf_s,
                             struct kemo_array_control *psf_a,
                             struct gl_strided_buffer *strided_buf,
                             struct gl_local_buffer_address *point_buf){
    long inum, iele, inod, k;
    int ipsf;
	int iflag;
	double xx_tri[9], rtp_patch[9];
	
	for(inum=0; inum<(ied_psf-ist_psf); inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 0];
			xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 1];
			xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 2];
		};
		iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITHREE*inum+k), strided_buf, point_buf);
			strided_buf->x_txur[0] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			strided_buf->x_txur[1] = 1.0 - rtp_patch[ITHREE*k+1] * ARCPI;
		};
	};
	return;
}

void set_psf_map_to_buf(long ist_psf, long ied_psf,
                        struct psf_data **psf_s,
                        struct kemo_array_control *psf_a,
                        struct gl_strided_buffer *strided_buf,
                        struct gl_local_buffer_address *point_buf){
    long inum, iele, inod, k;
    int ipsf, nd;
	double xx_tri[9], xyz_map[9];
	
	for(inum=0; inum<(ied_psf-ist_psf); inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 0];
			xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 1];
			xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITHREE*inum+k), strided_buf, point_buf);
			strided_buf->x_draw[0] = xyz_map[ITHREE*k  ];
			strided_buf->x_draw[1] = xyz_map[ITHREE*k+1];
			strided_buf->x_draw[2] = 0.0;
			
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[4*inod+nd];};
		};
	};
	return;
}


long count_psf_arrows_to_buf(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int inod;
	
	long inum_buf = 0;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
			if(psf_s->norm_nod[4*inod  ] != 0.0
						|| psf_s->norm_nod[4*inod+1] !=0.0
						|| psf_s->norm_nod[4*inod+2] !=0.0){
				inum_buf = inum_buf + ncorner;
			};
		};
	};
	
	return inum_buf;
}

long set_psf_arrows_to_buf(long ist_patch, int ncorner, 
                           struct psf_data *psf_s, struct psf_menu_val *psf_m, 
                           struct gl_strided_buffer *strided_buf,
                           struct gl_local_buffer_address *point_buf){
	double x_line[6], dir_line[6], color_line[8];
	double xyz[18*ncorner], nor[18*ncorner], col[24*ncorner];
	double dcolor[4];
	int num_wall;
	
	double v_tmp[3], v_xyz[3], x_rtp[3], d_mag;
    
	int inod, k, nd;
    long inum_buf, i;
	
	long icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	double radius = psf_m->vector_thick;
	double ascale = ONE / psf_m->scale_vect;
	
    struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);

    inum_buf = ist_patch;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
            if(psf_s->norm_nod[4*inod  ] != 0.0
               || psf_s->norm_nod[4*inod+1] !=0.0
               || psf_s->norm_nod[4*inod+2] !=0.0){
                for (k=0; k<3; k++) v_tmp[k] = psf_s->d_nod[inod*psf_s->ncomptot + icomp+k];
			
                if(psf_s->id_coord[psf_m->if_draw_psf]==1){
                    position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
                    sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
                } else if(psf_s->id_coord[psf_m->if_draw_psf]==2){
                    position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
                    cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
                } else {
                    for (k=0; k<3; k++) v_xyz[k] = v_tmp[k];
                };
			
				if(psf_m->ivect_tangential==TANGENTIAL_COMPONENT){
					for (k=0; k<3; k++) {
						v_xyz[k] = v_xyz[k] - psf_s->norm_nod[4*inod+k]
								* (  v_xyz[0]*psf_s->norm_nod[4*inod  ]
                                   + v_xyz[1]*psf_s->norm_nod[4*inod+1]
                                   + v_xyz[2]*psf_s->norm_nod[4*inod+2]);
					};
				};
				
				d_mag = sqrt(v_xyz[0]*v_xyz[0]+v_xyz[1]*v_xyz[1]+v_xyz[2]*v_xyz[2]);
				if(psf_m->vector_patch_color == RAINBOW_SURFACE){
					set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                           d_mag, dcolor);
				} else {
					for(nd=0;nd<4;nd++){dcolor[nd] = arrow_c[nd];};
				}
				
				for (k=0; k<3; k++){
					x_line[k  ] = psf_s->xyzw_viz[inod*IFOUR + k];
					x_line[k+3] = psf_s->xyzw_viz[inod*IFOUR + k] + v_xyz[k]*ascale;
					dir_line[k  ] =  v_xyz[k];
					dir_line[k+3] =  v_xyz[k];
				};
				for (k=0; k<4; k++){
					color_line[k  ] =  dcolor[k];
					color_line[k+4] =  dcolor[k];
				};
				
				num_wall = set_cone_vertex(ncorner, radius, x_line, dir_line, color_line,
							xyz, nor, col);
				
				for (i=0; i<3*num_wall; i++) {
                    set_node_stride_buffer((ITHREE*inum_buf+i), strided_buf, point_buf);
					for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xyz[3*i+nd];};
					for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = nor[3*i+nd];};
					for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = col[4*i+nd];};
				};
				inum_buf = inum_buf + num_wall;
			};
		};
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	
	return inum_buf;
}
