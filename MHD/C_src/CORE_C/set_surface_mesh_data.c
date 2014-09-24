/*
 *  set_surface_mesh_data.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/19.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_surface_mesh_data.h"


void set_surface_mesh_size(struct viewer_mesh *mesh_s){
	int inod, ist, ied;
	int ip, nd;
    double r_tmp;
	
	/* allocate grid data for draw */
	
	for (nd=0; nd<3; nd++) {
		for (ip=0; ip < mesh_s->num_pe_sf; ip++) {
			ist = mesh_s->inod_sf_stack[ip];
			ied = mesh_s->inod_sf_stack[ip+1];
			mesh_s->domain_max[ip][nd] = mesh_s->xx_view[ist][nd];
			mesh_s->domain_min[ip][nd] = mesh_s->xx_view[ist][nd];
			for (inod=ist+1; inod<ied; inod++) {
				if(mesh_s->xx_view[inod][nd] > mesh_s->domain_max[ip][nd])
                mesh_s->domain_max[ip][nd] = mesh_s->xx_view[inod][nd];
				if(mesh_s->xx_view[inod][nd] < mesh_s->domain_min[ip][nd])
                mesh_s->domain_min[ip][nd] = mesh_s->xx_view[inod][nd];
			};
			
			mesh_s->domain_center[ip][nd] = HALF * (mesh_s->domain_min[ip][nd]
													+ mesh_s->domain_max[ip][nd]);
		};
        
        mesh_s->xx_mesh_max[nd] = mesh_s->domain_max[0][nd];
        mesh_s->xx_mesh_min[nd] = mesh_s->domain_min[0][nd];
		for (ip=1; ip < mesh_s->num_pe_sf; ip++) {
            if(mesh_s->domain_max[ip][nd] > mesh_s->xx_mesh_max[nd])
            mesh_s->xx_mesh_max[nd] = mesh_s->domain_max[ip][nd];
            if(mesh_s->domain_min[ip][nd] < mesh_s->xx_mesh_min[nd])
            mesh_s->xx_mesh_min[nd] = mesh_s->domain_min[ip][nd];
        };
        mesh_s->mesh_center[nd] = HALF * (mesh_s->xx_mesh_min[nd]
                                          + mesh_s->xx_mesh_max[nd]);
	};
    
    mesh_s->rmax_mesh = HALF*(mesh_s->xx_mesh_max[0]-mesh_s->xx_mesh_min[0]);
	for (nd = 1; nd < 3; nd++) {
        r_tmp = HALF*(mesh_s->xx_mesh_max[nd]-mesh_s->xx_mesh_min[nd]);
        if(mesh_s->rmax_mesh < r_tmp) mesh_s->rmax_mesh = r_tmp;
	};
	return;
};

void set_surface_normal_4_each_node(struct viewer_mesh *mesh_s){
	int i, j, inum, jnum, k, k1;
	int iele, idir, inod, nd;
	double x_center[3];
	
	for (i=0; i<mesh_s->nsurf_domain_sf; i++) {
		iele = abs(mesh_s->isurf_domain_sf[i]) - 1;
		idir = abs(mesh_s->isurf_domain_sf[i]) / mesh_s->isurf_domain_sf[i];
        
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
			for (nd=0; nd<3; nd++){
				mesh_s->normal_domain[jnum][nd] = ((double) idir) * mesh_s->surf_norm_view[inum][nd];
				x_center[nd] = mesh_s->surf_center_view[inum][nd];
			};
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
					mesh_s->dist_nod_domain[jnum][k1]
                    = sqrt( (mesh_s->xx_view[inod][0] - x_center[0])
                           *(mesh_s->xx_view[inod][0] - x_center[0])
                           +(mesh_s->xx_view[inod][1] - x_center[1])
                           *(mesh_s->xx_view[inod][1] - x_center[1])
                           +(mesh_s->xx_view[inod][2] - x_center[2])
                           *(mesh_s->xx_view[inod][2] - x_center[2]) );
                    
					for (nd=0; nd<3; nd++){
						mesh_s->norm_nod_domain[jnum][nd+3*k1]
                        = ((double) idir) * mesh_s->surf_norm_view[inum][nd];
					};
				};
			}
		}
	};
    
	for (i=0; i<mesh_s->nele_ele_sf; i++) {
		iele = abs(mesh_s->ele_item_sf[i]) - 1;
		idir = abs(mesh_s->ele_item_sf[i]) / mesh_s->ele_item_sf[i];
		
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
            
			for (nd=0; nd<3; nd++){
				mesh_s->normal_ele_grp[jnum][nd] = ((double) idir) * mesh_s->surf_norm_view[inum][nd];
				x_center[nd] = mesh_s->surf_center_view[inum][nd];
			};
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
					mesh_s->dist_nod_ele_grp[jnum][k1]
					= sqrt( (mesh_s->xx_view[inod][0] - x_center[0])
						   *(mesh_s->xx_view[inod][0] - x_center[0])
						   +(mesh_s->xx_view[inod][1] - x_center[1])
						   *(mesh_s->xx_view[inod][1] - x_center[1])
						   +(mesh_s->xx_view[inod][2] - x_center[2])
						   *(mesh_s->xx_view[inod][2] - x_center[2]) );
                    
					for (nd=0; nd<3; nd++){
						mesh_s->norm_nod_ele_grp[jnum][nd+3*k1]
						= ((double) idir) * mesh_s->surf_norm_view[inum][nd];
					};
				};
			}
		}
	}
    
	for (i=0; i<mesh_s->nsurf_surf_sf; i++) {
		iele = abs(mesh_s->surf_item_sf[i]) - 1;
		idir = abs(mesh_s->surf_item_sf[i]) / mesh_s->surf_item_sf[i];
        
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
			
			for (nd=0; nd<3; nd++){
				mesh_s->normal_surf_grp[jnum][nd] = ((double) idir) * mesh_s->surf_norm_view[inum][nd];
				x_center[nd] = mesh_s->surf_center_view[inum][nd];
			};
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
					mesh_s->dist_nod_surf_grp[jnum][k1]
					= sqrt( (mesh_s->xx_view[inod][0] - x_center[0])
                           *(mesh_s->xx_view[inod][0] - x_center[0])
						   +(mesh_s->xx_view[inod][1] - x_center[1])
						   *(mesh_s->xx_view[inod][1] - x_center[1])
						   +(mesh_s->xx_view[inod][2] - x_center[2])
						   *(mesh_s->xx_view[inod][2] - x_center[2]) );
                    
					for (nd=0; nd<3; nd++){
						mesh_s->norm_nod_surf_grp[jnum][nd+3*k1]
						= ((double) idir) * mesh_s->surf_norm_view[inum][nd];
					};
				};
			}
		}
	}
	
	return;
}


