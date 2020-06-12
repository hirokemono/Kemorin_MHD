
/* set_PSF_patches_to_buf.c */

#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

static GLfloat arrow_c[4] = {0.8, 0.7, 0.6, 1.0};

int count_psf_nodes_to_buf(int ist_psf, int ied_psf){
	int num_patch = ied_psf - ist_psf;
	return num_patch;
};

void set_psf_nodes_to_buf(int ist_psf, int ied_psf, int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
    long inum, iele, inod;
    int k, nd, ipsf;
	
	for(inum=0; inum<(ied_psf-ist_psf); inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = psf_s[ipsf]->xx_viz[inod][nd];};
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[inod][nd];};
			if (shading_mode == SMOOTH_SHADE){
                for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = psf_s[ipsf]->norm_nod[inod][nd];};
			} else {
				for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = psf_s[ipsf]->norm_ele[iele][nd];};
			};
			if(psf_m[ipsf]->polygon_mode_psf == REVERSE_POLYGON){
				for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = -strided_buf->n_draw[nd];};
			};
		};
    };
    return;
}

void set_psf_textures_to_buf(int ist_psf, int ied_psf, struct psf_data **psf_s,
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
    long inum, iele, inod;
    int k, ipsf;
	int iflag;
	double xx_tri[9], rtp_patch[9];
	
	for(inum=0; inum<(ied_psf-ist_psf); inum++){
        ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
        iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		iflag = latitude_longitude_on_map(xx_tri, rtp_patch);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			strided_buf->x_txur[0] =  rtp_patch[ITHREE*k+2] * ARCPI * HALF;
			strided_buf->x_txur[1] = -rtp_patch[ITHREE*k+1] * ARCPI;
		};
	};
	return;
}

void set_psf_map_to_buf(int ist_psf, int ied_psf, struct psf_data **psf_s, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
    long inum, iele, inod;
    int ipsf, nd, k;
	double xx_tri[9], xyz_map[9];
	
	for(inum=0; inum<(ied_psf-ist_psf); inum++){
		ipsf = psf_a->ipsf_viz_far[inum+ist_psf]-1;
		iele = psf_a->iele_viz_far[inum+ist_psf]-1;
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			xx_tri[3*k  ] = psf_s[ipsf]->xx_viz[inod][0];
			xx_tri[3*k+1] = psf_s[ipsf]->xx_viz[inod][1];
			xx_tri[3*k+2] = psf_s[ipsf]->xx_viz[inod][2];
		};
		projection_patch_4_map(xx_tri, xyz_map);
		
		for (k = 0; k < ITHREE; k++) {
			inod = psf_s[ipsf]->ie_viz[iele][k] - 1;
			set_node_stride_VBO((ITHREE*inum+k), strided_buf);
			strided_buf->x_draw[0] = xyz_map[ITHREE*k  ];
			strided_buf->x_draw[1] = xyz_map[ITHREE*k+1];
			strided_buf->x_draw[2] = 0.0;
			
			for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = psf_s[ipsf]->color_nod[inod][nd];};
		};
	};
	return;
}


int count_psf_arrows_to_buf(int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m){
	int inod;
	
	int inum_buf = 0;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
			if(psf_s->norm_nod[inod][0] != 0.0
						|| psf_s->norm_nod[inod][1] !=0.0
						|| psf_s->norm_nod[inod][2] !=0.0){
				inum_buf = inum_buf + ncorner;
			};
		};
	};
	
	return inum_buf;
}

int set_psf_arrows_to_buf(int ist_patch, int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m, 
			struct gl_strided_buffer *strided_buf) {
	double x_line[6], dir_line[6], color_line[8];
	double xyz[18*ncorner], nor[18*ncorner], col[24*ncorner];
	double dcolor[4];
	int num_wall, inum_buf;
	
	double v_tmp[3], v_xyz[3], x_rtp[3], d_mag;
    
	int inod, i, k, nd;
	
	int icomp = psf_s->istack_comp[psf_m->if_draw_psf];
	double radius = psf_m->vector_thick;
	double ascale = ONE / psf_m->scale_vect;
	
	inum_buf = ist_patch;
	for (inod = 0; inod < psf_s->nnod_viz; inod++) {
		if (inod % psf_m->increment_vect == 0) {
            if(psf_s->norm_nod[inod][0] != 0.0
               || psf_s->norm_nod[inod][1] !=0.0
               || psf_s->norm_nod[inod][2] !=0.0){
                for (k=0; k<3; k++) v_tmp[k] = psf_s->d_nod[inod][icomp+k];
			
                if(psf_s->id_coord[psf_m->if_draw_psf]==1){
                    position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
                    sph_vector_to_xyz_vect(x_rtp[1], x_rtp[2], v_tmp, v_xyz);
                } else if(psf_s->id_coord[psf_m->if_draw_psf]==2){
                    position_2_sph_c(IONE, psf_s->xx_viz[inod], x_rtp);
                    cyl_vector_to_xyz_vect(x_rtp[2], v_tmp, v_xyz);
                } else {
                    for (k=0; k<3; k++) v_xyz[k] = v_tmp[k];
                };
			
				if(psf_m->ivect_tangential==TANGENTIAL_COMPONENT){
					for (k=0; k<3; k++) {
						v_xyz[k] = v_xyz[k] - psf_s->norm_nod[inod][k]
								* (  v_xyz[0]*psf_s->norm_nod[inod][0]
									+ v_xyz[1]*psf_s->norm_nod[inod][1]
									+ v_xyz[2]*psf_s->norm_nod[inod][2]);
					};
				};
				
				d_mag = sqrt(v_xyz[0]*v_xyz[0]+v_xyz[1]*v_xyz[1]+v_xyz[2]*v_xyz[2]);
				if(psf_m->vector_patch_color == RAINBOW_SURFACE){
					set_rainbow_color_code(psf_m->cmap_psf_comp[psf_m->icomp_draw_psf], d_mag, dcolor);
				} else {
					for(nd=0;nd<4;nd++){dcolor[nd] = arrow_c[nd];};
				}
				
				for (k=0; k<3; k++){
					x_line[k  ] = psf_s->xx_viz[inod][k];
					x_line[k+3] = psf_s->xx_viz[inod][k] + v_xyz[k]*ascale;
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
					set_node_stride_VBO((ITHREE*inum_buf+i), strided_buf);
					for(nd=0;nd<3;nd++){strided_buf->x_draw[nd] = xyz[3*i+nd];};
					for(nd=0;nd<3;nd++){strided_buf->n_draw[nd] = nor[3*i+nd];};
					for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = col[4*i+nd];};
				};
				inum_buf = inum_buf + num_wall;
			};
		};
	};
	
	return inum_buf;
}
