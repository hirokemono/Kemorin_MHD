/*
 *  take_normal_surf_mesh_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/23.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "take_normal_surf_mesh_c.h"


static void set_center_of_each_surface(int nnod_patch, int *ie_sf, int *ie_each, double *xx,
									   double xx_surf[3]){
	double xx_quad[4][3];
	int i1, k, nd;
	
	for (k=0; k<nnod_patch; k++) {
		i1 = ie_sf[(ie_each[k] - 1)]-1;
		for(nd = 0; nd < 3; nd++){xx_quad[k][nd] = xx[4*i1+nd];};
	};
	
	if(nnod_patch == IFOUR){
		cal_center_4_quad_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_quad[3],  xx_surf);
	} else {
		cal_center_4_triangle_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_surf);
	};
	
	return;
};

static double cal_normal_4_each_surface(int nnod_patch, int *ie_sf, int *ie_each, 
										double *xx, double normal[3]){
	double size;
	double xx_quad[4][3];
	int i1, k, nd;
	
	for (k=0; k<nnod_patch; k++) {
		i1 = ie_sf[(ie_each[k] - 1)]-1;
		for (nd = 0; nd < 3; nd++){xx_quad[k][nd] = xx[4*i1+nd];};
	};
	
	if(nnod_patch == IFOUR){
		size = cal_normal_4_quad_c(xx_quad[0], xx_quad[1], xx_quad[2], xx_quad[3],  normal);
	} else {
		size = cal_normal_4_triangle_c(xx_quad[0], xx_quad[1], xx_quad[2], normal);
	};
	
	return size;
};

static void ave_normal_4_two_tris(double size1, double size2, double norm1[3], double norm2[3]){
	double size;
	int nd;
	
	if( (size1+size2) ==ZERO){
		for (nd = 0; nd < 3; nd++){
			norm1[nd] = ZERO;
			norm2[nd] = ZERO;
		};
	} else {
		for (nd = 0; nd < 3; nd++){norm1[nd] = (size1*norm1[nd] + size2*norm2[nd]);};
		size = sqrt(norm1[0]*norm1[0] + norm1[1]*norm1[1] + norm1[2]*norm1[2]);
		
		for (nd = 0; nd < 3; nd++){
			norm1[nd] = norm1[nd] / size;
			norm2[nd] = norm1[nd];
		};
	};
	return;
}



void center_of_mesh_triangles(struct viewer_mesh *mesh_s,
                              double *surf_center_view){
    int iele, j, jnum;
    
    /*#pragma omp parallel for private(iele,j,jnum)*/
    for(iele = 0; iele < mesh_s->nsurf_viewer; iele++){
        for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
            jnum = j + iele * mesh_s->nsurf_each_tri;
            set_center_of_each_surface(ITHREE, &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf*iele],
                                       &mesh_s->node_quad_2_linear_tri[3*j], mesh_s->xx_view,
                                       &surf_center_view[4*jnum]);
        };
    };
    return;
};

void take_normal_surf_mesh_c(struct viewer_mesh *mesh_s){
	int iele, j, jnum;

/*#pragma omp parallel for private(iele,j,jnum)*/
	for(iele = 0; iele < mesh_s->nsurf_viewer; iele++){
		for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
			jnum = j + iele * mesh_s->nsurf_each_tri;
			mesh_s->surf_size_view[jnum]
			= cal_normal_4_each_surface(ITHREE, &mesh_s->ie_sf_viewer[mesh_s->nnod_4_surf*iele],
                                        &mesh_s->node_quad_2_linear_tri[3*j], mesh_s->xx_view,
                                        &mesh_s->surf_norm_view[4*jnum]);
		};
	};
	
	if (mesh_s->nnod_4_surf == 8){
/*#pragma omp parallel for private(j, jnum, s_norm)*/
		for(iele = 0; iele < mesh_s->nsurf_viewer; iele++){
			jnum = iele * mesh_s->nsurf_each_tri;
			ave_normal_4_two_tris(mesh_s->surf_size_view[jnum],
                                  mesh_s->surf_size_view[jnum+1],
                                  &mesh_s->surf_norm_view[4*jnum],
                                  &mesh_s->surf_norm_view[4*jnum+4]);
		}
	} else {
/*#pragma omp parallel for private(j, jnum, s_norm)*/
		for(iele = 0; iele < mesh_s->nsurf_viewer; iele++){
			for (j = 0; j < mesh_s->nsurf_each_tri/2; j++) {
				jnum = 2*j + iele * mesh_s->nsurf_each_tri;
				ave_normal_4_two_tris(mesh_s->surf_size_view[jnum],
                                      mesh_s->surf_size_view[jnum+1],
                                      &mesh_s->surf_norm_view[4*jnum],
                                      &mesh_s->surf_norm_view[4*jnum+4]);
			}
		}
	};
}
