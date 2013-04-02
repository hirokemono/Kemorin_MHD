/*
 *  take_normal_surf_mesh_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/23.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "take_normal_surf_mesh_c.h"


static void set_center_of_each_surface(int nnod_patch, int *ie_sf, int *ie_each, double **xx, 
									   double xx_surf[3]){
	double xx_quad[4][3];
	int i1, k, nd;
	
	for (k=0; k<nnod_patch; k++) {
		i1 = ie_sf[(ie_each[k] - 1)]-1;
		for (nd = 0; nd < 3; nd++){xx_quad[k][nd] = xx[i1][nd];};
	};
	
	if(nnod_patch == IFOUR){
		cal_center_4_quad_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_quad[3],  xx_surf);
	} else {
		cal_center_4_triangle_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_surf);
	};
	
	return;
};

static double cal_normal_4_each_surface(int nnod_patch, int *ie_sf, int *ie_each, 
										double **xx, double normal[3]){
	double size;
	double xx_quad[4][3];
	int i1, k, nd;
	
	for (k=0; k<nnod_patch; k++) {
		i1 = ie_sf[(ie_each[k] - 1)]-1;
		for (nd = 0; nd < 3; nd++){xx_quad[k][nd] = xx[i1][nd];};
	};
	
	if(nnod_patch == IFOUR){
		size = cal_normal_4_quad_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_quad[3],  normal);
	} else {
		size = cal_normal_4_triangle_c(xx_quad[0], xx_quad[1], xx_quad[2], normal);
	};
	
	return size;
};


void take_normal_surf_mesh_c(struct viewer_mesh *mesh_s){
	int iele, j, jnum;

/*#pragma omp parallel for private(jnum)*/
	for(iele = 0; iele < mesh_s->surfpetot_viewer; iele++){
		jnum = iele * mesh_s->nsurf_each_sf;
		set_center_of_each_surface(IFOUR, mesh_s->ie_sf_viewer[iele], 
								   &mesh_s->node_quad_2_linear_sf[0],
								   mesh_s->xx_view, mesh_s->surf_center_view[jnum]);
		mesh_s->surf_size_view[jnum]
			= cal_normal_4_each_surface(IFOUR, mesh_s->ie_sf_viewer[iele], 
										&mesh_s->node_quad_2_linear_sf[0], 
										mesh_s->xx_view, mesh_s->surf_norm_view[jnum]);
	};

	if (mesh_s->nnod_4_surf == 9){
/*#pragma omp parallel for private(j, jnum)*/
		for(iele = 0; iele < mesh_s->surfpetot_viewer; iele++){
			for (j = 1; j < mesh_s->nsurf_each_sf; j++) {
				jnum = j + iele * mesh_s->nsurf_each_sf;
				set_center_of_each_surface(IFOUR, mesh_s->ie_sf_viewer[iele], 
										   &mesh_s->node_quad_2_linear_sf[4*j], mesh_s->xx_view,
										   mesh_s->surf_center_view[jnum]);
				mesh_s->surf_size_view[jnum]
					= cal_normal_4_each_surface(IFOUR, mesh_s->ie_sf_viewer[iele], 
										   &mesh_s->node_quad_2_linear_sf[4*j], mesh_s->xx_view,
										   mesh_s->surf_norm_view[jnum]);
			};
		};

	} else if (mesh_s->nnod_4_surf == 8){
		for(iele = 0; iele < mesh_s->surfpetot_viewer; iele++){
/*#pragma omp parallel for private(j, jnum)*/
			for (j = 1; j < mesh_s->nsurf_each_sf; j++) {
				jnum = j + iele * mesh_s->nsurf_each_sf;
				set_center_of_each_surface(ITHREE, mesh_s->ie_sf_viewer[iele], 
										   &mesh_s->node_quad_2_linear_sf[4*j], mesh_s->xx_view,
										   mesh_s->surf_center_view[jnum]);
				mesh_s->surf_size_view[jnum]
					= cal_normal_4_each_surface(ITHREE, mesh_s->ie_sf_viewer[iele], 
												 &mesh_s->node_quad_2_linear_sf[4*j], mesh_s->xx_view,
												 mesh_s->surf_norm_view[jnum]);
			};
		};
 
	};
	return;
}
