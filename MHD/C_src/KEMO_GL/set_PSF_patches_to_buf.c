
/* set_PSF_patches_to_buf.c */

#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

static float arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

long count_psf_nodes_to_buf(long ist_psf, long ied_psf){
	return (ied_psf - ist_psf);
};

long set_psf_nodes_to_buf(long ipatch_in, long ist_psf, long ied_psf, int shading_mode, 
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long lk, inum, iele, inod;
    int nd, ipsf;
	
    long ipatch = ipatch_in;
	for(inum=ist_psf;inum<ied_psf;inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
		for (lk = 0; lk < ITHREE; lk++) {
			inod = psf_s[ipsf]->ie_viz[iele][lk] - 1;
			
            set_node_stride_buffer((ITHREE*ipatch+lk), strided_buf, &point_buf);
			for(nd=0;nd<3;nd++){
                strided_buf->v_buf[nd+point_buf.igl_xyzw] = psf_s[ipsf]->xyzw_viz[IFOUR*inod + nd];
            };
			for(nd=0;nd<4;nd++){
                strided_buf->v_buf[nd+point_buf.igl_color] = psf_s[ipsf]->color_nod[IFOUR*inod+nd];
            };
			if (shading_mode == SMOOTH_SHADE){
                for(nd=0;nd<3;nd++){
                    strided_buf->v_buf[nd+point_buf.igl_norm] = psf_s[ipsf]->norm_nod[IFOUR*inod+nd];
                };
			} else {
				for(nd=0;nd<3;nd++){
                    strided_buf->v_buf[nd+point_buf.igl_norm] = psf_s[ipsf]->norm_ele[IFOUR*iele+nd];
                };
			};
			if(psf_m[ipsf]->polygon_mode_psf == REVERSE_POLYGON){
				for(nd=0;nd<3;nd++){
                    strided_buf->v_buf[nd+point_buf.igl_norm] = -strided_buf->v_buf[nd+point_buf.igl_norm];
                };
			};
		};
        ipatch = ipatch + 1;
    };
    return ipatch;
}

long set_psf_textures_to_buf(long ist_texture, long ist_psf, long ied_psf,
                             struct psf_data **psf_s,
                             struct kemo_array_control *psf_a,
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long inum, iele, inod, k;
    int ipsf;
	int iflag;
	double xx_tri[9], rtp_patch[9];
	
    long ipatch = ist_texture;
	for(inum=ist_psf; inum<ied_psf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum]-1;
        iele = psf_a->iele_viz_far[inum]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 0];
			xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 1];
			xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 2];
		};
		iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITHREE*ipatch+k), strided_buf, &point_buf);
			strided_buf->v_buf[point_buf.igl_txur  ] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			strided_buf->v_buf[point_buf.igl_txur+1] = 1.0 - rtp_patch[ITHREE*k+1] * ARCPI;
		};
        ipatch = ipatch + 1;
	};
	return ipatch;
}

long set_psf_map_to_buf(long ist_patch, long ist_psf, long ied_psf,
                        struct psf_data **psf_s,
                        struct kemo_array_control *psf_a,
                        struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long inum, iele, inod, k;
    int ipsf;
	double xx_tri[9], xyz_map[9];
	
    long ipatch = ist_patch;
	for(inum=ist_psf; inum<ied_psf; inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 0];
			xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 1];
			xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
            set_node_stride_buffer((ITHREE*ipatch+k), strided_buf, &point_buf);
			strided_buf->v_buf[  point_buf.igl_xyzw] = xyz_map[ITHREE*k  ];
			strided_buf->v_buf[1+point_buf.igl_xyzw] = xyz_map[ITHREE*k+1];
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.0;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			
            strided_buf->v_buf[  point_buf.igl_color] = psf_s[ipsf]->color_nod[4*inod  ];
            strided_buf->v_buf[1+point_buf.igl_color] = psf_s[ipsf]->color_nod[4*inod+1];
            strided_buf->v_buf[2+point_buf.igl_color] = psf_s[ipsf]->color_nod[4*inod+2];
            strided_buf->v_buf[3+point_buf.igl_color] = psf_s[ipsf]->color_nod[4*inod+3];
		};
        ipatch = ipatch + 1;
	};
	return ipatch;
}


long add_num_psf_arrows(long ist_patch, long ist, long ied, int ncorner,
                        struct psf_data *psf_s, struct psf_menu_val *psf_m){
    long inod;
    long inum_buf = ist_patch;
    for(inod = ist; inod < ied; inod++){
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


long set_psf_arrows_to_buf(long ist_patch, long ist, long ied,
                           int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m,
                           struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	double x_line[6], dir_line[6], color_line[8];
	double xyzw[24*ncorner], norm[24*ncorner], col[24*ncorner];
	double dcolor[4];
	int num_wall;
	
	double v_tmp[3], v_xyz[3], x_rtp[3], d_mag;
    
	int k, nd;
    long inod, inum_buf, i;
	
	long icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	double radius = psf_m->vector_thick;
	double ascale = ONE / psf_m->scale_vect;
	
    struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);

    inum_buf = ist_patch;
    for(inod = ist; inod < ied; inod++){
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
                                           xyzw, norm, col);
				
				for (i=0; i<3*num_wall; i++) {
                    set_node_stride_buffer((ITHREE*inum_buf+i), strided_buf, &point_buf);
					for(nd=0;nd<4;nd++){
                        strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*i+nd];
                        strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*i+nd];
                        strided_buf->v_buf[nd+point_buf.igl_color] = col[4*i+nd];
                    };
				};
				inum_buf = inum_buf + num_wall;
			};
		};
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	
	return inum_buf;
}

