
/* set_PSF_patches_to_buf.c */

#include "set_PSF_patches_to_buf.h"

#define ARCPI 0.318309886

int count_psf_nodes_to_buf(int ist_psf, int ied_psf){
	int num_patch = ied_psf - ist_psf;
	return num_patch;
};

void set_psf_nodes_to_buf(int ist_psf, int ied_psf, int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m, 
			struct kemo_array_control *psf_a, struct gl_strided_buffer *strided_buf){
	int inum, iele, inod, k, nd, ipsf;
	
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
	int inum, iele, inod, k, ipsf;
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
	int inum, iele, inod, ipsf, nd, k;
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
