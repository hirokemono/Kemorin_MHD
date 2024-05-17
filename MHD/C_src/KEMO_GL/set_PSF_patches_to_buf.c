
/* set_PSF_patches_to_buf.c */

#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

static float arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

long count_psf_nodes_to_buf(long ist_psf, long ied_psf){
	return (ied_psf - ist_psf);
};

static void set_psf_nodes_to_tri(long ipsf, long iele,
                                 int shading_mode, int polygon_mode_psf,
                                 struct psf_data **psf_s, double xyzw_tri[12],
                                 double norm_tri[12], double color_tri[12]){
    long inod, nd;
    
    for(long k = 0; k < ITHREE; k++) {
        inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
        for(nd=0;nd<4;nd++){xyzw_tri[4*k+nd] = psf_s[ipsf]->xyzw_viz[IFOUR*inod + nd];};
        for(nd=0;nd<4;nd++){color_tri[4*k+nd] = psf_s[ipsf]->color_nod[IFOUR*inod+nd];};
        if (shading_mode == SMOOTH_SHADE){
            for(nd=0;nd<4;nd++){norm_tri[4*k+nd] = psf_s[ipsf]->norm_nod[IFOUR*inod+nd];};
        } else {
            for(nd=0;nd<4;nd++){norm_tri[4*k+nd] = psf_s[ipsf]->norm_ele[IFOUR*iele+nd];};
        };
        if(polygon_mode_psf == REVERSE_POLYGON){
            for(nd=0;nd<4;nd++){norm_tri[4*k+nd] = -norm_tri[4*k+nd];};
        };
        xyzw_tri[4*k+3] = 1.0;
        norm_tri[4*k+3] = 1.0;
    };
    return;
}

static void set_psf_textures_to_tri(long ipsf, long iele,
                                    struct psf_data **psf_s,
                                    double xy_txur[6]){
    long inod, k;
    double xx_tri[9], rtp_patch[9];
    
    for(k = 0; k < ITHREE; k++) {
        inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
        xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 0];
        xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 1];
        xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[inod*IFOUR + 2];
    };
    int iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
    
    for(k = 0; k < ITHREE; k++) {
        xy_txur[2*k  ] =       rtp_patch[ITHREE*k+2] * ARCPI * HALF;
        xy_txur[2*k+1] = 1.0 - rtp_patch[ITHREE*k+1] * ARCPI;
    };
    return;
}

static void set_psf_map_to_tri(long ipsf, long iele, struct psf_data **psf_s,
                               double xyzw_tri[12], double color_tri[12]){
    long inod, k;
    double xx_tri[9], xyz_map[9];
    for (k = 0; k < ITHREE; k++) {
        inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
        xx_tri[3*k  ] = psf_s[ipsf]->xyzw_viz[4*inod + 0];
        xx_tri[3*k+1] = psf_s[ipsf]->xyzw_viz[4*inod + 1];
        xx_tri[3*k+2] = psf_s[ipsf]->xyzw_viz[4*inod + 2];
        
        color_tri[4*k  ] = psf_s[ipsf]->color_nod[4*inod  ];
        color_tri[4*k+1] = psf_s[ipsf]->color_nod[4*inod+1];
        color_tri[4*k+2] = psf_s[ipsf]->color_nod[4*inod+2];
        color_tri[4*k+3] = psf_s[ipsf]->color_nod[4*inod+3];
    };
    
    projection_patch_4_map(xx_tri, xyz_map);
    
    for (k = 0; k < ITHREE; k++) {
        xyzw_tri[4*k  ] = xyz_map[ITHREE*k  ];
        xyzw_tri[4*k+1] = xyz_map[ITHREE*k+1];
        xyzw_tri[4*k+2] = 0.0;
        xyzw_tri[4*k+3] = 1.0;
    };
    return;
}


long set_psf_nodes_to_buf(long ipatch_in, long ist_psf, long ied_psf, int shading_mode, 
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf){
    double xyzw_tri[12], norm_tri[12], color_tri[12];
    long iele, ipsf;
    long ipatch = ipatch_in;
	for(long inum=ist_psf;inum<ied_psf;inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
        set_psf_nodes_to_tri(ipsf, iele, shading_mode, psf_m[ipsf]->polygon_mode_psf,
                             psf_s, xyzw_tri, norm_tri, color_tri);
        ipatch = set_patch_strided_buffer(ipatch, xyzw_tri, norm_tri, color_tri,
                                          strided_buf);
    };
    return ipatch;
}

long set_psf_textures_to_buf(long ist_texture, long ist_psf, long ied_psf,
                             struct psf_data **psf_s,
                             struct kemo_array_control *psf_a,
                             struct gl_strided_buffer *strided_buf){
    double xy_txur[6];
    long ipsf, iele;
	
    long ipatch = ist_texture;
	for(long inum=ist_psf; inum<ied_psf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum]-1;
        iele = psf_a->iele_viz_far[inum]-1;
        set_psf_textures_to_tri(ipsf, iele, psf_s, xy_txur);
        ipatch = set_textur_to_buf(ipatch, xy_txur, strided_buf);
	};
	return ipatch;
}

long set_psf_map_to_buf(long ist_patch, long ist_psf, long ied_psf,
                        struct psf_data **psf_s,
                        struct kemo_array_control *psf_a,
                        struct gl_strided_buffer *strided_buf){
    double xyzw_tri[12], color_tri[12];
    double norm_tri[12] = {0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0};

    long inum, iele, inod, k;
    int ipsf;
	double xx_tri[9], xyz_map[9];
	
    long ipatch = ist_patch;
	for(inum=ist_psf; inum<ied_psf; inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
        set_psf_map_to_tri(ipsf, iele, psf_s, xyzw_tri, color_tri);
        ipatch = set_patch_strided_buffer(ipatch, xyzw_tri, norm_tri, color_tri,
                                          strided_buf);
	};
	return ipatch;
}


static void set_line_for_psf_arrow(int icomp, long inod, 
                                   struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                   double xyzw_line[8], double dir_line[8], 
                                   double color_line[8]){
    int nd;
    double v_xyz[3], v_tmp[3], x_rtp[3];
    double dcolor[4];
    
	double ascale = ONE / psf_m->scale_vect;
    
    for (nd=0; nd<3; nd++) v_tmp[nd] = psf_s->d_nod[inod*psf_s->ncomptot + icomp+nd];

    if(psf_s->id_coord[psf_m->if_draw_psf]==1){
        position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
        sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
    } else if(psf_s->id_coord[psf_m->if_draw_psf]==2){
        position_2_sph_c(IONE, &psf_s->xyzw_viz[inod*IFOUR], x_rtp);
        cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
    } else {
        for (nd=0; nd<3; nd++) v_xyz[nd] = v_tmp[nd];
    };

    if(psf_m->ivect_tangential==TANGENTIAL_COMPONENT){
        for (nd=0; nd<3; nd++) {
            v_xyz[nd] = v_xyz[nd] - psf_s->norm_nod[4*inod+nd]
                    * (  v_xyz[0]*psf_s->norm_nod[4*inod  ]
                       + v_xyz[1]*psf_s->norm_nod[4*inod+1]
                       + v_xyz[2]*psf_s->norm_nod[4*inod+2]);
        };
    };
    
    for (nd=0; nd<3; nd++){
        xyzw_line[nd  ] = psf_s->xyzw_viz[4*inod + nd];
        xyzw_line[nd+4] = psf_s->xyzw_viz[4*inod + nd] + v_xyz[nd]*ascale;
        dir_line[nd  ] =  v_xyz[nd];
        dir_line[nd+4] =  v_xyz[nd];
    };
    xyzw_line[3] = 1.0;
    xyzw_line[7] = 1.0;
    dir_line[3] = 1.0;
    dir_line[7] = 1.0;
    return;
}

long add_num_psf_arrows(long ist_cone, long ist, long ied, int ncorner,
                        struct psf_data *psf_s, struct psf_menu_val *psf_m){
    long inod;
    long inum_cone = ist_cone;
    for(inod = ist; inod < ied; inod++){
        if (inod % psf_m->increment_vect == 0) {
            if(psf_s->norm_nod[4*inod  ] != 0.0
                        || psf_s->norm_nod[4*inod+1] !=0.0
                        || psf_s->norm_nod[4*inod+2] !=0.0){
                inum_cone = inum_cone + 1;
            };
        };
    };
    
    return inum_cone;
}


long set_psf_arrows_to_buf(long ist_cone, long ist, long ied,
                           int ncorner, double radius,
                           struct psf_data *psf_s, struct psf_menu_val *psf_m,
                           struct gl_strided_buffer *strided_buf){
	double xyzw_line[8], dir_line[8], color_line[8];
	double xyzw[24*ncorner], norm[24*ncorner], col[24*ncorner];
        
    struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
    
    long inod, nd;
    double d_mag;
    
    long inum_cone = ist_cone;
    for(inod = ist; inod < ied; inod++){
		if (inod % psf_m->increment_vect == 0) {
            if(   psf_s->norm_nod[4*inod  ] != 0.0
               || psf_s->norm_nod[4*inod+1] !=0.0
               || psf_s->norm_nod[4*inod+2] !=0.0){
                set_line_for_psf_arrow(psf_s->istack_comp[psf_m->if_draw_psf], 
                                       inod, psf_s, psf_m,
                                       xyzw_line, dir_line, color_line);
                        
                if(psf_m->vector_patch_color == RAINBOW_SURFACE){
                    d_mag = sqrt(dir_line[0]*dir_line[0] + dir_line[1]*dir_line[1] + dir_line[2]*dir_line[2]);
                    set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                           d_mag, color_line);
                } else {
                    for(nd=0;nd<4;nd++){color_line[nd] = arrow_c[nd];};
                }
                for (nd=0; nd<4; nd++){color_line[nd+4] =  color_line[nd];};
                
                inum_cone = set_cone_strided_buffer(inum_cone, ncorner, radius,
                                                    xyzw_line, dir_line, color_line,
                                                    strided_buf);
			};
		};
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	
	return inum_cone;
}
