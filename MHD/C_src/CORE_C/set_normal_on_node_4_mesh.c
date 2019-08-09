/*
 *  set_normal_on_node_4_mesh.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/21.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_normal_on_node_4_mesh.h"

#define SPLIT_EDGE 0.9

static double *icou_nod_tmp;
static double *dist_nod_tmp;
static double **norm_nod_tmp;

static void alloc_norm_nod_tmp(int ntot_nod){
	int i;
	icou_nod_tmp = (double *)calloc(ntot_nod,sizeof(double));
	dist_nod_tmp = (double *)calloc(ntot_nod,sizeof(double));
	norm_nod_tmp = (double **)calloc(ntot_nod,sizeof(double *));
	for (i = 0; i < ntot_nod; i++){
		norm_nod_tmp[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
}

static void dealloc_norm_nod_tmp(int ntot_nod){
	int i;

	for (i = 0; i < ntot_nod; i++) free(norm_nod_tmp[i]);
	free(norm_nod_tmp);
	free(dist_nod_tmp);
	free(icou_nod_tmp);

	return;
}


static void refine_normal_on_node_4_grp(struct viewer_mesh *mesh_s, int ntot_nod, int ist, int ied, 
									 int *item_grp, double **dist_nod, double **norm_nod){
	int j, i, inum, iele, idir, inod, jnum, k1, k, nd;
	double size, cos_ang;

	for (inod = 0; inod < ntot_nod; inod++){
		icou_nod_tmp[inod] = ZERO;
		dist_nod_tmp[inod] = ZERO;
		for(nd=0;nd<3;nd++) norm_nod_tmp[inod][nd] = ZERO;
	};
	 
	for (i=ist; i<ied; i++) {
		iele = abs(item_grp[i]) - 1;
		idir = abs(item_grp[i]) / item_grp[i];
	 
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
					icou_nod_tmp[inod] = icou_nod_tmp[inod] + 1;
					dist_nod_tmp[inod] = dist_nod_tmp[inod] + dist_nod[jnum][k1];
				};
			}
		}
	}
	 
	 
	 
	for (i=ist; i<ied; i++) {
		iele = abs(item_grp[i]) - 1;
		idir = abs(item_grp[i]) / item_grp[i];

		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
				
					for(nd=0;nd<3;nd++){
						norm_nod_tmp[inod][nd] = norm_nod_tmp[inod][nd]
							+ (dist_nod_tmp[inod] - dist_nod[jnum][k1])
							* norm_nod[jnum][nd+3*k1];
					};
				};
			}
		}
	}
	 
	 
	for (inod = 0; inod < ntot_nod; inod++){
		size = sqrt(norm_nod_tmp[inod][0]*norm_nod_tmp[inod][0]
					+ norm_nod_tmp[inod][1]*norm_nod_tmp[inod][1]
					+ norm_nod_tmp[inod][2]*norm_nod_tmp[inod][2]);
		for(nd=0;nd<3;nd++) norm_nod_tmp[inod][nd] = norm_nod_tmp[inod][nd] / size;
	};
	 
	for (i=ist; i<ied; i++) {
		iele = abs(item_grp[i]) - 1;
		idir = abs(item_grp[i]) / item_grp[i];
	 
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
			inum = j + iele * mesh_s->nsurf_each_tri;
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[iele][k] - 1;
	 
					cos_ang =  norm_nod_tmp[inod][0] * norm_nod[jnum][  3*k1]
							 + norm_nod_tmp[inod][1] * norm_nod[jnum][1+3*k1]
							 + norm_nod_tmp[inod][2] * norm_nod[jnum][2+3*k1];
					if(cos_ang > SPLIT_EDGE){
						for(nd=0;nd<3;nd++) norm_nod[jnum][nd+3*k1] =  norm_nod_tmp[inod][nd];
					};
				};
			}
		}
	 }

	return;
}

void set_normal_on_node_4_mesh(struct viewer_mesh *mesh_s){
	int i, ip, ip_st;
	
	alloc_norm_nod_tmp(mesh_s->nnod_viewer);
	
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
								 mesh_s->isurf_stack_domain_sf[ip], mesh_s->isurf_stack_domain_sf[ip+1],
								 mesh_s->isurf_domain_sf, mesh_s->dist_nod_domain, mesh_s->norm_nod_domain);
	}
	/* ! draw element group */
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
		for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
			ip_st = ip + i * mesh_s->num_pe_sf;
			refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
									 mesh_s->ele_stack_sf[ip_st], mesh_s->ele_stack_sf[ip_st+1],
									 mesh_s->ele_item_sf, mesh_s->dist_nod_ele_grp, mesh_s->norm_nod_ele_grp);
		};
	};
	/* ! draw surface group */
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
		for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
			ip_st = ip + i * mesh_s->num_pe_sf;
			refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
									 mesh_s->surf_stack_sf[ip_st], mesh_s->surf_stack_sf[ip_st+1],
									 mesh_s->surf_item_sf, mesh_s->dist_nod_surf_grp, mesh_s->norm_nod_surf_grp);
		};
	};
	
	dealloc_norm_nod_tmp(mesh_s->nnod_viewer);
	
	return;
}
