
/* set_PSF_patches_to_buf.c */

#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

static float arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

long count_psf_nodes_to_buf(long ist_psf, long ied_psf){
	return (ied_psf - ist_psf);
};

static void set_psf_nodes_to_tri(long ipsf, long iele,
                                 struct psf_data **psf_s, struct psf_normals **psf_n,
                                 double xyzw_tri[12], double norm_tri[12],
                                 double color_tri[12]){
    long inod, nd;
    long k;
    for(k = 0; k < ITHREE; k++) {
        inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
        for(nd=0;nd<4;nd++){xyzw_tri[4*k+nd] = psf_s[ipsf]->xyzw_viz[IFOUR*inod + nd];};
        for(nd=0;nd<4;nd++){color_tri[4*k+nd] = psf_s[ipsf]->color_nod[IFOUR*inod+nd];};
        for(nd=0;nd<4;nd++){norm_tri[4*k+nd] = psf_n[ipsf]->norm_ele[IFOUR*iele+nd];};
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
    latitude_longitude_on_map(xx_tri, rtp_patch);
    
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


long set_psf_nodes_to_buf(long ipatch_in, long ist_nod, long num,
                          struct psf_data *psf_s, struct psf_normals *psf_n,
                          struct gl_strided_buffer *strided_buf){
    long inum_nod = ipatch_in;
    inum_nod =  set_nodes_strided_buffer(inum_nod, num,
                                         &psf_s->xyzw_viz[IFOUR*ist_nod],
                                         &psf_n->norm_nod[IFOUR*ist_nod],
                                         &psf_s->color_nod[IFOUR*ist_nod],
                                         &psf_n->rt_viz[ITWO*ist_nod],
                                         strided_buf);
    return inum_nod;
}

long set_psf_patch_indices_to_buf(long ipatch_in, long ist_psf, long ied_psf,
                                  struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                  struct gl_index_buffer *index_buf){
    long iele, ipsf;
    long ipatch = ipatch_in;
    long inum, k;
    for(inum=ist_psf;inum<ied_psf;inum++){
        ipsf = psf_a->ipsf_viz_far[inum]-1;
        iele = psf_a->iele_viz_far[inum]-1;
        for(k = 0; k < ITHREE; k++) {
            index_buf->ie_buf[ITHREE*ipatch+k] = (unsigned int) psf_a->istack_all_psf_node[ipsf]
                                                + (unsigned int) psf_s[ipsf]->ie_viz[iele][k] - 1;
        };
        ipatch = ipatch + 1;
    };
    return ipatch;
}

long set_psf_patches_to_buf(long ipatch_in, long ist_psf, long ied_psf,
                            struct psf_data **psf_s,
                            struct psf_normals **psf_n,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *strided_buf){
    double xyzw_tri[12], norm_tri[12], color_tri[12];
    long iele, ipsf, inum;
    long ipatch = ipatch_in;
	for(inum=ist_psf;inum<ied_psf;inum++){
		ipsf = psf_a->ipsf_viz_far[inum]-1;
		iele = psf_a->iele_viz_far[inum]-1;
        set_psf_nodes_to_tri(ipsf, iele, psf_s, psf_n,
                             xyzw_tri, norm_tri, color_tri);
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
    long ipsf, iele, inum;
	
    long ipatch = ist_texture;
	for(inum=ist_psf; inum<ied_psf; inum++){
        ipsf = psf_a->ipsf_viz_far[inum]-1;
        iele = psf_a->iele_viz_far[inum]-1;
        set_psf_textures_to_tri(ipsf, iele, psf_s, xy_txur);
        ipatch = set_patch_textur_to_buf(ipatch, xy_txur, strided_buf);
	};
	return ipatch;
}


long set_map_nodes_to_buf(long ipatch_in, long ist_nod, long num,
                          struct psf_data *psf_s,
                          struct psf_normals *psf_n,
                          struct gl_strided_buffer *strided_buf){
    double rtpw[4] =   {1.0, 0.0, 0.0, 1.0};
    double map_xy[4] = {0.0, 0.0, 0.0002, 1.0};;
    double normal[4] = {0.0, 0.0, 1.0, 1.0};
    long inum_nod = ipatch_in;
    long inod;
    for(inod=ist_nod;inod<ist_nod+num ;inod++){
        rtpw[1] = psf_n->rt_viz[ITWO*inod  ];
        rtpw[2] = psf_n->rt_viz[ITWO*inod+1];
        aitoff_c(IONE, &rtpw[0], &map_xy[0]);

        inum_nod =  set_nodes_strided_buffer(inum_nod, IONE,
                                             map_xy, normal,
                                             &psf_s->color_nod[IFOUR*inod],
                                             &psf_n->rt_viz[ITWO*inod],
                                             strided_buf);
    }
    return inum_nod;
}

long set_map_patch_to_buf(long ist_patch, long ist_psf, long ied_psf,
                          struct psf_data **psf_s,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf){
    double xyzw_tri[12], color_tri[12];
    double norm_tri[12] = {0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0,
                           0.0, 0.0, 1.0, 1.0};

    long inum, iele, ipsf;
	
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


long add_num_psf_arrows(long ist_cone, long ist, long ied, int ncorner,
                        struct psf_data *psf_s, struct psf_normals *psf_n,
                        struct psf_menu_val *psf_m){
    long inod;
    long inum_cone = ist_cone;
    for(inod = ist; inod < ied; inod++){
        if (inod % psf_m->increment_vect == 0) {
            if(psf_n->norm_nod[4*inod  ] != 0.0
                        || psf_n->norm_nod[4*inod+1] !=0.0
                        || psf_n->norm_nod[4*inod+2] !=0.0){
                inum_cone = inum_cone + 1;
            };
        };
    };
    
    return inum_cone;
}


long set_psf_arrows_to_buf(long ist_cone, long ist, long ied,
                           int ncorner, double radius,
                           struct psf_data *psf_s,
                           struct psf_normals *psf_n,
                           struct psf_menu_val *psf_m,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_index_buffer *index_buf){
	double xyzw_line[8], dir_line[8], color_line[8];
        
    struct colormap_params *cmap_s = psf_m->cmap_viz_comp[psf_m->icomp_draw_viz];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
    
    long inod, nd;
    double d_mag;
    
    long inum_cone = ist_cone;
    for(inod = ist; inod < ied; inod++){
		if (inod % psf_m->increment_vect == 0) {
            if(   psf_n->norm_nod[4*inod  ] != 0.0
               || psf_n->norm_nod[4*inod+1] !=0.0
               || psf_n->norm_nod[4*inod+2] !=0.0){
                set_line_for_psf_arrow((int) psf_s->istack_comp[psf_m->if_draw_viz], 
                                       inod, psf_s, psf_n, psf_m,
                                       xyzw_line, dir_line);
                        
                if(psf_m->vector_patch_color == COLORED_BY_DATA){
                    d_mag = sqrt(dir_line[0]*dir_line[0] + dir_line[1]*dir_line[1] + dir_line[2]*dir_line[2]);
                    set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                           d_mag, color_line);
                } else {
                    for(nd=0;nd<4;nd++){color_line[nd] = arrow_c[nd];};
                }
                color_line[3] = 1.0;
                for (nd=0; nd<4; nd++){color_line[nd+4] =  color_line[nd];};
                
                inum_cone = set_cone_node_index_buffer(inum_cone, ncorner, radius,
                                                       xyzw_line, dir_line, color_line,
                                                       strided_buf, index_buf);
			};
		};
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	
	return inum_cone;
}
