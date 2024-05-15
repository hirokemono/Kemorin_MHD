/*
 *  t_psf_edge_connect_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/11/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "t_psf_edge_connect_c.h"

static void alloc_edge_connect_psf(const int nnod_4_edge, const int nedge_4_surf, const long nele_viz, 
							struct psf_edge_data_c *psf_edge){
	int i;
	
	psf_edge->nnod_p_edge = nnod_4_edge;
	psf_edge->nedge_p_surf = nedge_4_surf;
	
	psf_edge->ie_edge = (long **) malloc(psf_edge->nedge_viewer*sizeof(long *));
	if(psf_edge->ie_edge == NULL) {
		printf("malloc error for psf_edge->ie_edge \n");
		exit(0);
	}
	for(i=0;i<psf_edge->nedge_viewer;i++){
		psf_edge->ie_edge[i] = (long *)calloc(psf_edge->nnod_p_edge, sizeof(long));
		if((psf_edge->ie_edge[i]) == NULL){
			printf("malloc error for psf_edge->ie_edge[%d]\n", i);
			exit(0);
		};
	};
/*
	psf_edge->iedge_gl_view = (long *)calloc(psf_edge->nedge_viewer, sizeof(long));
	if((psf_edge->iedge_gl_view) == NULL){
		printf("malloc error for psf_edge->iedge_gl_view[%d]\n", i);
		exit(0);
	};
*/
	psf_edge->iedge_4_sf = (long **) malloc(nele_viz*sizeof(long *));
	if(psf_edge->iedge_4_sf == NULL) {
		printf("malloc error for psf_edge->iedge_4_sf \n");
		exit(0);
	}
	for(i=0;i<nele_viz;i++){
		psf_edge->iedge_4_sf[i] = (long *)calloc(nedge_4_surf, sizeof(long));
		if((psf_edge->iedge_4_sf[i]) == NULL){
			printf("malloc error for psf_edge->iedge_4_sf[%d]\n", i);
			exit(0);
		};
	};
	
	return;
};

static struct psf_edge_geometry * alloc_edge_position_psf(struct psf_edge_data_c *psf_edge){
    struct psf_edge_geometry *psf_edge_xyz
        = (struct psf_edge_geometry *) malloc(sizeof(struct psf_edge_geometry *));
    if(psf_edge_xyz == NULL) {
        printf("malloc error for psf_edge_geometry \n");
        exit(0);
    }

    psf_edge_xyz->xyzw_edge = (double *) malloc(4*psf_edge->nedge_viewer*sizeof(double));
	if(psf_edge_xyz->xyzw_edge == NULL) {
		printf("malloc error for psf_edge_xyz->xyzw_edge \n");
		exit(0);
	}

    psf_edge_xyz->edge_norm = (double *) malloc(4*psf_edge->nedge_viewer*sizeof(double));
	if(psf_edge_xyz->edge_norm == NULL) {
		printf("malloc error for psf_edge_xyz->edge_norm \n");
		exit(0);
	}
    psf_edge_xyz->edge_dir = (double *) malloc(4*psf_edge->nedge_viewer*sizeof(double));
	if(psf_edge_xyz->edge_dir == NULL) {
		printf("malloc error for psf_edge_xyz->edge_dir \n");
		exit(0);
	}

    psf_edge_xyz->edge_len = (double *) calloc(psf_edge->nedge_viewer, sizeof(double));
	if((psf_edge_xyz->edge_len) == NULL){
		printf("malloc error for psf_edge_xyz->edge_len\n");
		exit(0);
	};
	return psf_edge_xyz;
};

static void dealloc_edge_connect_psf(const long nele_viz, struct psf_edge_data_c *psf_edge){
	int i;
	
    for(i=0;i<nele_viz;i++){free(psf_edge->iedge_4_sf[i]);};
	free(psf_edge->iedge_4_sf);
//	free(psf_edge->iedge_gl_view);
	
	for(i=0;i<psf_edge->nedge_viewer;i++){free(psf_edge->ie_edge[i]);};
	free(psf_edge->ie_edge);
	return;
};

static void dealloc_edge_position_psf(struct psf_edge_geometry *psf_edge_xyz){
	free(psf_edge_xyz->xyzw_edge);
	free(psf_edge_xyz->edge_norm);
	free(psf_edge_xyz->edge_dir);
	free(psf_edge_xyz->edge_len);
	return;
};


static long count_num_edges_by_sf_c(struct sum_hash_tbl_c *ed_sf_tbl){
	long numedge;
	
	long k1, ist, ied, ihash;
	
	numedge = 0;
	for(ihash=0;ihash<ed_sf_tbl->iend_hash;ihash++){
		ist = ed_sf_tbl->istack_hash[ihash];
		ied = ed_sf_tbl->istack_hash[ihash+1];
		for(k1=ist;k1<ied;k1++){
			if(ed_sf_tbl->iflag_hash[k1] == k1+1){numedge = numedge + 1;};
		};
	};
	return numedge;
};

static void set_edges_connect_by_sf_c(const int nnod_4_surf, long **ie_viz,
							   struct sum_hash_tbl_c *ed_sf_tbl,
							   struct psf_edge_data_c *psf_edge){
	int i, j;
	long ihash, iedge, isurf, is, jsurf, js, k1, k2, kk, ist, ied;
	
	iedge = 0;
	for(ihash=0;ihash<ed_sf_tbl->iend_hash;ihash++){
		ist = ed_sf_tbl->istack_hash[ihash];
		ied = ed_sf_tbl->istack_hash[ihash+1];
		for(k1=ist;k1<ied;k1++){
			if(ed_sf_tbl->iflag_hash[k1] == k1+1){
				iedge = iedge + 1;
				isurf = ed_sf_tbl->id_hash[0][k1];
				is =    ed_sf_tbl->id_hash[1][k1];
				psf_edge->iedge_4_sf[isurf][is] = iedge;
				for(i=0;i<psf_edge->nnod_p_edge;i++){
					j = ed_sf_tbl->node_on_edge_tri[is][i];
					psf_edge->ie_edge[iedge-1][i] = ie_viz[isurf][j];
				};
			};
		};
	};
	
	for(ihash=0;ihash<ed_sf_tbl->iend_hash;ihash++){
		ist = ed_sf_tbl->istack_hash[ihash];
		ied = ed_sf_tbl->istack_hash[ihash+1];
		for(k1=ist;k1<ied;k1++){
			if(ed_sf_tbl->iflag_hash[k1] != k1+1){
                kk = (int) ed_sf_tbl->iflag_hash[k1];
				k2 = labs(kk);
				isurf = ed_sf_tbl->id_hash[0][k1];
				is =    ed_sf_tbl->id_hash[1][k1];
				jsurf = ed_sf_tbl->id_hash[0][k2-1];
				js =    ed_sf_tbl->id_hash[1][k2-1];
                psf_edge->iedge_4_sf[isurf][is] 
 		               = psf_edge->iedge_4_sf[jsurf][js] * ((int) (kk / k2));
			};
		};
	};
	return;
};


static void set_edge_position_4_sf_c(double *xyzw_viz, struct psf_edge_data_c *psf_edge,
                                     struct psf_edge_geometry *psf_edge_xyz){
	long iedge, i0, i1;
	
	for(iedge=0;iedge<psf_edge->nedge_viewer;iedge++){
		i0 = psf_edge->ie_edge[iedge][0] - 1;
		i1 = psf_edge->ie_edge[iedge][1] - 1;
        psf_edge_xyz->xyzw_edge[4*iedge  ] = 0.5 * (xyzw_viz[i1*IFOUR + 0] + xyzw_viz[i0*IFOUR + 0]);
        psf_edge_xyz->xyzw_edge[4*iedge+1] = 0.5 * (xyzw_viz[i1*IFOUR + 1] + xyzw_viz[i0*IFOUR + 1]);
        psf_edge_xyz->xyzw_edge[4*iedge+2] = 0.5 * (xyzw_viz[i1*IFOUR + 2] + xyzw_viz[i0*IFOUR + 2]);
        psf_edge_xyz->xyzw_edge[4*iedge+3] = 1.0;
	};
	return;
};

static void set_edge_direction_4_sf_c(double *xyzw_viz, struct psf_edge_data_c *psf_edge,
                                      struct psf_edge_geometry *psf_edge_xyz){
	long iedge, i0, i1;
	
	for(iedge=0;iedge<psf_edge->nedge_viewer;iedge++){
		i0 = psf_edge->ie_edge[iedge][0] - 1;
		i1 = psf_edge->ie_edge[iedge][1] - 1;
        psf_edge_xyz->edge_dir[4*iedge  ] = xyzw_viz[i1*IFOUR + 0] - xyzw_viz[i0*IFOUR + 0];
        psf_edge_xyz->edge_dir[4*iedge+1] = xyzw_viz[i1*IFOUR + 1] - xyzw_viz[i0*IFOUR + 1];
        psf_edge_xyz->edge_dir[4*iedge+2] = xyzw_viz[i1*IFOUR + 2] - xyzw_viz[i0*IFOUR + 2];
	};
	
	for(iedge=0;iedge<psf_edge->nedge_viewer;iedge++){
        psf_edge_xyz->edge_len[iedge] = sqrt(  pow(psf_edge_xyz->edge_dir[4*iedge  ],2.0)
                                             + pow(psf_edge_xyz->edge_dir[4*iedge+1],2.0)
                                             + pow(psf_edge_xyz->edge_dir[4*iedge+2],2.0));
		
		if(psf_edge_xyz->edge_len[iedge] == 0.0){
            psf_edge_xyz->edge_dir[4*iedge+0] = 0.0;
            psf_edge_xyz->edge_dir[4*iedge+1] = 0.0;
            psf_edge_xyz->edge_dir[4*iedge+2] = 1.0;
		}else{
            psf_edge_xyz->edge_dir[4*iedge  ] = psf_edge_xyz->edge_dir[4*iedge  ]
                                                / psf_edge_xyz->edge_len[iedge];
            psf_edge_xyz->edge_dir[4*iedge+1] = psf_edge_xyz->edge_dir[4*iedge+1]
                                                / psf_edge_xyz->edge_len[iedge];
            psf_edge_xyz->edge_dir[4*iedge+2] = psf_edge_xyz->edge_dir[4*iedge+2]
                                                / psf_edge_xyz->edge_len[iedge];
		};
        psf_edge_xyz->edge_dir[4*iedge+3] = 1.0;
	};
	return;
};

static void set_edge_normal_4_sf_c(double *norm_nod, struct psf_edge_data_c *psf_edge,
                                   struct psf_edge_geometry *psf_edge_xyz){
	long iedge, i0, i1;
	double norm_size;
	
	for(iedge=0;iedge<psf_edge->nedge_viewer;iedge++){
		i0 = psf_edge->ie_edge[iedge][0] - 1;
		i1 = psf_edge->ie_edge[iedge][1] - 1;
        psf_edge_xyz->edge_norm[4*iedge  ] = 0.5 * (norm_nod[4*i1+0] + norm_nod[4*i0+0]);
        psf_edge_xyz->edge_norm[4*iedge+1] = 0.5 * (norm_nod[4*i1+1] + norm_nod[4*i0+1]);
        psf_edge_xyz->edge_norm[4*iedge+2] = 0.5 * (norm_nod[4*i1+2] + norm_nod[4*i0+2]);
	};
	
	for(iedge=0;iedge<psf_edge->nedge_viewer;iedge++){
		norm_size = sqrt(pow(psf_edge_xyz->edge_norm[4*iedge  ],2.0)
					   + pow(psf_edge_xyz->edge_norm[4*iedge+1],2.0)
					   + pow(psf_edge_xyz->edge_norm[4*iedge+2],2.0));
		
        psf_edge_xyz->edge_norm[4*iedge  ]
				= psf_edge_xyz->edge_norm[4*iedge  ] / norm_size;
        psf_edge_xyz->edge_norm[4*iedge+1]
				= psf_edge_xyz->edge_norm[4*iedge+1] / norm_size;
        psf_edge_xyz->edge_norm[4*iedge+2]
				= psf_edge_xyz->edge_norm[4*iedge+2] / norm_size;
        psf_edge_xyz->edge_norm[4*iedge+3] = 1.0;
	};
	return;
};

struct psf_edge_data_c * init_psf_edge_data_c(void){
	struct psf_edge_data_c *psf_edge = (struct psf_edge_data_c *) malloc(sizeof(struct psf_edge_data_c));
	if(psf_edge == NULL) {
		printf("malloc error for psf_edge_data_c \n");
		exit(0);
	}
	return psf_edge;
};

struct psf_edge_data_c * init_all_edge_4_psf(const long nnod_viz, const long nele_viz,
											 const int nnod_4_ele_viz, long **ie_viz,
											 double *xyzw_viz, double *norm_nod){
	struct psf_edge_data_c *psf_edge;
	
	int nnod_4_edge =    2;
	int nedge_triangle = 3;
	if(nnod_4_ele_viz != 3){
		printf("PSF data does not consist of triangles\n");
		return NULL;
	}
	
	/*!   set hash data for edge elements using sum of local node ID */
	struct sum_hash_tbl_c *ed_sf_tbl
			= init_sum_hash(nnod_viz, nele_viz, nnod_4_ele_viz, 
							nnod_4_edge);
	clear_sum_hash(ed_sf_tbl);
	set_edge_hash_4_triangle(nedge_triangle, nele_viz, nnod_4_ele_viz, 
							 ie_viz, ed_sf_tbl);
	mark_all_edges_on_triangle(nnod_4_ele_viz, ie_viz, ed_sf_tbl);
	
	
	psf_edge = init_psf_edge_data_c();
	psf_edge->nedge_viewer = count_num_edges_by_sf_c(ed_sf_tbl);
	alloc_edge_connect_psf(nnod_4_edge, nedge_triangle, nele_viz, psf_edge);
	set_edges_connect_by_sf_c(nnod_4_ele_viz, ie_viz, ed_sf_tbl, psf_edge);
/*
    struct psf_edge_geometry *psf_edge_xyz = alloc_edge_position_psf(psf_edge);
	set_edge_position_4_sf_c(xyzw_viz, psf_edge, psf_edge_xyz);
	set_edge_direction_4_sf_c(xyzw_viz, psf_edge, psf_edge_xyz);
	set_edge_normal_4_sf_c(norm_nod, psf_edge, psf_edge_xyz);
*/
/*
    printf("nnod nele %ld %ld \n", nnod_viz, nele_viz);
    long i;
    for(i=0;i<nele_viz;i++){
		printf("patch %ld, %ld %ld %ld\n", i, ie_viz[i][0], ie_viz[i][1], ie_viz[i][2]);
    }
    for(i=0;i<psf_edge->nedge_viewer;i++){
        printf("edge %ld, %ld %ld\n", i, psf_edge->ie_edge[i][0], psf_edge->ie_edge[i][1]);
    };
    for(i=0;i<nele_viz;i++){
        printf("iedge_4_sf %ld, %ld %ld %ld\n", i, 
               psf_edge->iedge_4_sf[i][0], psf_edge->iedge_4_sf[i][1], psf_edge->iedge_4_sf[i][2]);
    };
*/
    
    dealloc_sum_hash(ed_sf_tbl);

    return psf_edge;
};

void dealloc_edge_data_4_psf(const long nele_viz, struct psf_edge_data_c *psf_edge){
//	dealloc_edge_position_psf(psf_edge_xyz);
	dealloc_edge_connect_psf(nele_viz, psf_edge);
	free(psf_edge);
	return;
};
