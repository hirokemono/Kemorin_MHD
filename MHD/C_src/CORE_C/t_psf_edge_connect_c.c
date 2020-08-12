/*
 *  t_psf_edge_connect_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/11/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "t_psf_edge_connect_c.h"

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
	long ihash, iedge, isurf, is, jsurf, js, k1, k2, ist, ied;
	
	iedge = 0;
	for(ihash=0;ihash<ed_sf_tbl->iend_hash;ihash++){
		ist = ed_sf_tbl->istack_hash[ihash];
		ied = ed_sf_tbl->istack_hash[ihash+1];
		for(k1=ist;k1<ied;k1++){
			if(ed_sf_tbl->iflag_hash[k1] == k1+1){
				iedge = iedge + 1;
				isurf = ed_sf_tbl->id_hash[0][k1];
				is =    ed_sf_tbl->id_hash[1][k1];
				psf_edge->iedge_4_sf[is][isurf] = iedge;
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
				k2 = (int) labs(ed_sf_tbl->iflag_hash[k1]) - 1;
				isurf = ed_sf_tbl->id_hash[0][k1];
				is =    ed_sf_tbl->id_hash[1][k1];
				jsurf = ed_sf_tbl->id_hash[0][k2];
				js =    ed_sf_tbl->id_hash[1][k2];
				psf_edge->iedge_4_sf[is][isurf] 
					= psf_edge->iedge_4_sf[js][jsurf] 
					* ((int) ((ed_sf_tbl->iflag_hash[k1]+1) / (k2+1)));
			};
		};
	};
	return;
};

static void alloc_edge_data_4_sf_c(const int nnod_4_edge, const int nedge_4_surf, const long nele_viz, 
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
		psf_edge->ie_edge[i] = (long *)calloc(psf_edge->nnod_p_edge, sizeof(int));
		if((psf_edge->ie_edge[i]) == NULL){
			printf("malloc error for psf_edge->ie_edge[%d]\n", i);
			exit(0);
		};
	};
	
	psf_edge->iedge_gl_view = (long *)calloc(psf_edge->nedge_viewer, sizeof(long));
	if((psf_edge->iedge_gl_view) == NULL){
		printf("malloc error for psf_edge->iedge_gl_view[%d]\n", i);
		exit(0);
	};
	
	psf_edge->iedge_4_sf = (long **) malloc(nedge_4_surf*sizeof(long *));
	if(psf_edge->iedge_4_sf == NULL) {
		printf("malloc error for psf_edge->iedge_4_sf \n");
		exit(0);
	}
	for(i=0;i<nedge_4_surf;i++){
		psf_edge->iedge_4_sf[i] = (long *)calloc(nele_viz, sizeof(long));
		if((psf_edge->iedge_4_sf[i]) == NULL){
			printf("malloc error for psf_edge->iedge_4_sf[%d]\n", i);
			exit(0);
		};
	};
	
	return;
};

struct psf_edge_data_c * init_psf_edge_data_c(){
	struct psf_edge_data_c *psf_edge = (struct psf_edge_data_c *) malloc(sizeof(struct psf_edge_data_c));
	if(psf_edge == NULL) {
		printf("malloc error for psf_edge_data_c \n");
		exit(0);
	}
	return psf_edge;
};

struct psf_edge_data_c * init_all_edge_4_psf(const long nnod_viz, const long nele_viz,
											 const int nnod_4_ele_viz, long **ie_viz){
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
	alloc_edge_data_4_sf_c(nnod_4_edge, nedge_triangle, nele_viz, psf_edge);
	set_edges_connect_by_sf_c(nnod_4_ele_viz, ie_viz, ed_sf_tbl, psf_edge);
	
	dealloc_sum_hash(ed_sf_tbl);
 /*   
    printf("nnod nele %ld %ld \n", nnod_viz, nele_viz);
    long i;
    for(i=0;i<nele_viz;i++){
        if(ie_viz[i][0] > 73727 || ie_viz[i][1] > 73727  || ie_viz[i][2] > 73727){
       		 printf("tri %ld %ld %ld %ld\n", i, ie_viz[i][0], ie_viz[i][1], ie_viz[i][2]);
        };
    }
    for(i=0;i<psf_edge->nedge_viewer;i++){
        printf("edge %ld %ld %ld\n", i, psf_edge->ie_edge[i][0], psf_edge->ie_edge[i][1]);
    };
  */
	return psf_edge;
};

void dealloc_edge_data_4_psf(struct psf_edge_data_c *psf_edge){
	int i;
	
	for(i=0;i<psf_edge->nedge_p_surf;i++){free(psf_edge->iedge_4_sf[i]);};
	free(psf_edge->iedge_4_sf);
	free(psf_edge->iedge_gl_view);
	
	for(i=0;i<psf_edge->nnod_p_edge;i++){free(psf_edge->ie_edge[i]);};
	free(psf_edge->ie_edge);
	
	free(psf_edge);
	return;
};
