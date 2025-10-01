/*
 *  set_surface_mesh_data.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/19.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_surface_mesh_data.h"

#define SPLIT_EDGE 0.9

struct normal_nod_work * alloc_norm_nod_tmp(const struct viewer_mesh *mesh_s){
    struct normal_nod_work *wk_norm = (struct normal_nod_work *) malloc(sizeof(struct normal_nod_work));
    if(wk_norm == NULL){
        printf("malloc error for wk_norm\n");
        exit(0);
    };

    long num;
    num = mesh_s->nsurf_domain_sf * mesh_s->nsurf_each_tri;
    wk_norm->dist_nod_domain = (double *)calloc(3*num,sizeof(double));
    if(wk_norm->dist_nod_domain == NULL) {
        printf("malloc error for dist_nod_domain\n");
        exit(0);
    }
    num = mesh_s->nele_ele_sf * mesh_s->nsurf_each_tri;
    wk_norm->dist_nod_ele_grp = (double *)calloc(3*num,sizeof(double));
    if(wk_norm->dist_nod_ele_grp == NULL) {
        printf("malloc error for dist_nod_ele_grp\n");
        exit(0);
    }
    num = mesh_s->nsurf_surf_sf * mesh_s->nsurf_each_tri;
    wk_norm->dist_nod_surf_grp = (double *)calloc(3*num,sizeof(double));
    if(wk_norm->dist_nod_surf_grp == NULL) {
        printf("malloc error for dist_nod_surf_grp\n");
        exit(0);
    }



    wk_norm->icou_nod_tmp = (double *)calloc(mesh_s->nnod_viewer,sizeof(double));
    if(wk_norm->icou_nod_tmp == NULL){
        printf("malloc error for icou_nod_tmp\n");
        exit(0);
    };

    wk_norm->dist_nod_tmp = (double *)calloc(mesh_s->nnod_viewer,sizeof(double));
    if(wk_norm->dist_nod_tmp == NULL){
        printf("malloc error for dist_nod_tmp\n");
        exit(0);
    };

    wk_norm->norm_nod_tmp = (double *)calloc(4*mesh_s->nnod_viewer,sizeof(double *));
    if(wk_norm->norm_nod_tmp == NULL){
        printf("malloc error for norm_nod_tmp\n");
        exit(0);
    };
	return wk_norm;
}

void dealloc_norm_nod_tmp(struct normal_nod_work *wk_norm){
	free(wk_norm->norm_nod_tmp);
	free(wk_norm->dist_nod_tmp);
	free(wk_norm->icou_nod_tmp);

    free(wk_norm->dist_nod_surf_grp);
    free(wk_norm->dist_nod_ele_grp);
    free(wk_norm->dist_nod_domain);

    free(wk_norm);
	return;
}


void set_surface_mesh_size(struct viewer_mesh *mesh_s){
	int inod, ist, ied;
	int ip, nd;
    double r_tmp;
	
	/* allocate grid data for draw */
	
	for (nd=0; nd<3; nd++) {
		for (ip=0; ip < mesh_s->num_pe_sf; ip++) {
			ist = mesh_s->inod_sf_stack[ip];
			ied = mesh_s->inod_sf_stack[ip+1];
			mesh_s->domain_max[4*ip+nd] = mesh_s->xx_view[4*ist+nd];
			mesh_s->domain_min[4*ip+nd] = mesh_s->xx_view[4*ist+nd];
			for (inod=ist+1; inod<ied; inod++) {
				if(mesh_s->xx_view[4*inod+nd] > mesh_s->domain_max[4*ip+nd])
                        mesh_s->domain_max[4*ip+nd] = mesh_s->xx_view[4*inod+nd];
				if(mesh_s->xx_view[4*inod+nd] < mesh_s->domain_min[4*ip+nd])
                        mesh_s->domain_min[4*ip+nd] = mesh_s->xx_view[4*inod+nd];
			};
			
			mesh_s->domain_center[4*ip+nd] = HALF * (mesh_s->domain_min[4*ip+nd]
                                                   + mesh_s->domain_max[4*ip+nd]);
		};
        
        mesh_s->xx_mesh_max[nd] = mesh_s->domain_max[nd];
        mesh_s->xx_mesh_min[nd] = mesh_s->domain_min[nd];
		for (ip=1; ip < mesh_s->num_pe_sf; ip++) {
            if(mesh_s->domain_max[4*ip+nd] > mesh_s->xx_mesh_max[nd])
                    mesh_s->xx_mesh_max[nd] = mesh_s->domain_max[4*ip+nd];
            if(mesh_s->domain_min[4*ip+nd] < mesh_s->xx_mesh_min[nd])
                    mesh_s->xx_mesh_min[nd] = mesh_s->domain_min[4*ip+nd];
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

void set_normal_4_each_node(int i, int iele, int idir, struct viewer_mesh *mesh_s,
                            double *normal, double *norm_nod, double *distance){
    int j, k, k1;
    int inod, inum, jnum, nd;
    double x_center[3];
    
    for (j=0; j<mesh_s->nsurf_each_tri; j++) {
        jnum = j + i * mesh_s->nsurf_each_tri;
        inum = j + iele * mesh_s->nsurf_each_tri;
        for (nd=0; nd<3; nd++){
            normal[4*jnum+nd] = ((double) idir) * mesh_s->surf_norm_view[4*inum+nd];
            x_center[nd] = mesh_s->surf_center_view[4*inum+nd];
        };
        for (k1=0; k1<ITHREE; k1++) {
            if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
                k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
                inod = mesh_s->ie_sf_viewer[k + mesh_s->nnod_4_surf*iele] - 1;
                distance[3*jnum+k1]
                  = sqrt( (mesh_s->xx_view[4*inod+0] - x_center[0])
                          *(mesh_s->xx_view[4*inod+0] - x_center[0])
                         +(mesh_s->xx_view[4*inod+1] - x_center[1])
                          *(mesh_s->xx_view[4*inod+1] - x_center[1])
                         +(mesh_s->xx_view[4*inod+2] - x_center[2])
                          *(mesh_s->xx_view[4*inod+2] - x_center[2]) );
                
                for (nd=0; nd<3; nd++){
                    norm_nod[12*jnum+4*k1+nd]
                        = ((double) idir) * mesh_s->surf_norm_view[4*inum+nd];
                };
            };
        };
    };
	return;
}


void refine_normal_on_node_4_grp(struct viewer_mesh *mesh_s, int ntot_nod, int ist, int ied,
                                 int *item_grp, double *dist_nod, double *norm_nod, 
                                 struct normal_nod_work *wk_norm){
	int j, i, inum, iele, idir, inod, jnum, k1, k, nd;
	double size, cos_ang;

    for (inod = 0; inod < ntot_nod; inod++){
        wk_norm->icou_nod_tmp[inod] = ZERO;
        wk_norm->dist_nod_tmp[inod] = ZERO;
        for(nd=0;nd<4;nd++){wk_norm->norm_nod_tmp[4*inod+nd] = ZERO;};
	};
	 
	for (i=ist; i<ied; i++) {
		iele = abs(item_grp[i]) - 1;
/*		idir = abs(item_grp[i]) / item_grp[i]; */
	 
		for (j=0; j<mesh_s->nsurf_each_tri; j++) {
			jnum = j + i * mesh_s->nsurf_each_tri;
/*			inum = j + iele * mesh_s->nsurf_each_tri; */
			for (k1=0; k1<ITHREE; k1++) {
				if(mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] > 0){
					k = mesh_s->node_quad_2_linear_tri[ITHREE*j+k1] - 1;
					inod = mesh_s->ie_sf_viewer[k + mesh_s->nnod_4_surf*iele] - 1;
                    wk_norm->icou_nod_tmp[inod] = wk_norm->icou_nod_tmp[inod] + 1;
                    wk_norm->dist_nod_tmp[inod] = wk_norm->dist_nod_tmp[inod]
                                                 + dist_nod[3*jnum+k1];
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
					inod = mesh_s->ie_sf_viewer[k + mesh_s->nnod_4_surf*iele] - 1;
				
					for(nd=0;nd<3;nd++){
                        wk_norm->norm_nod_tmp[4*inod+nd] = wk_norm->norm_nod_tmp[4*inod+nd]
							+ (wk_norm->dist_nod_tmp[inod] - dist_nod[3*jnum+k1])
							* norm_nod[12*jnum+4*k1+nd];
					};
				};
			}
		}
	}
	 
	 
	for (inod = 0; inod < ntot_nod; inod++){
		size = sqrt(wk_norm->norm_nod_tmp[4*inod+0] * wk_norm->norm_nod_tmp[4*inod+0]
                  + wk_norm->norm_nod_tmp[4*inod+1] * wk_norm->norm_nod_tmp[4*inod+1]
                  + wk_norm->norm_nod_tmp[4*inod+2] * wk_norm->norm_nod_tmp[4*inod+2]);
        for(nd=0;nd<3;nd++){
            wk_norm->norm_nod_tmp[4*inod+nd] = wk_norm->norm_nod_tmp[4*inod+nd] / size;
        };
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
					inod = mesh_s->ie_sf_viewer[k + mesh_s->nnod_4_surf*iele] - 1;
	 
					cos_ang =  wk_norm->norm_nod_tmp[4*inod+0] * norm_nod[12*jnum+4*k1  ]
							 + wk_norm->norm_nod_tmp[4*inod+1] * norm_nod[12*jnum+4*k1+1]
							 + wk_norm->norm_nod_tmp[4*inod+2] * norm_nod[12*jnum+4*k1+2];
					if(cos_ang > SPLIT_EDGE){
                        for(nd=0;nd<3;nd++){
                            norm_nod[12*jnum+4*k1+nd] =  wk_norm->norm_nod_tmp[4*inod+nd];
                        };
					};
				};
			}
		}
	 }

	return;
}

void set_each_patch_group_id(int num_pe_sf, int nsurf_each_tri,
                             int igrp, long istart_group,
                             int *istack_grp, int *item_grp,
                             int *item_mesh_patch,
                             int *igroup_mesh_patch){
    int ist, ied, icou;
    int ip_st = igrp * num_pe_sf;
    int ip;
    for(ip = 0; ip < num_pe_sf; ip++){
        ist = istack_grp[ip_st+ip];
        ied = istack_grp[ip_st+ip+1];
        for(icou=ist;icou<ied; icou++){
            item_mesh_patch[icou] = item_grp[icou];
        }
        
        ist = nsurf_each_tri * istack_grp[ip_st+ip];
        ied = nsurf_each_tri * istack_grp[ip_st+ip+1];
        for(icou=ist;icou<ied; icou++){
            igroup_mesh_patch[icou] = ip + ip_st + (int) istart_group;
        }
    }
    return;
}
