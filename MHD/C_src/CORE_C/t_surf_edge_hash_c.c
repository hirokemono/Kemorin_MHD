/*
 *  t_surf_edge_hash_c.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/11/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "t_surf_edge_hash_c.h"

static int node_on_edge_tri[3][2] = {{0,1}, {1,2}, {2,0}};

struct sum_hash_tbl_c * init_sum_hash(const long numnod, const long nele, 
									  const int num_4_ele, const int nnod_4_edge){
	int i;
	struct sum_hash_tbl_c *h_tbl;
	if((h_tbl = (struct sum_hash_tbl_c *) malloc(sizeof(struct sum_hash_tbl_c))) == NULL) {
		printf("malloc error for sum_hash_tbl_c \n");
		exit(0);
	}
	
	for(i=0;i<3;i++){
		h_tbl->node_on_edge_tri[i][0] = node_on_edge_tri[i][0];
		h_tbl->node_on_edge_tri[i][1] = node_on_edge_tri[i][1];
	};
	
	h_tbl->ntot_id =  numnod * ((long) nnod_4_edge);
	h_tbl->iend_hash = h_tbl->ntot_id;
	
	if((h_tbl->istack_hash = (long *)calloc(h_tbl->ntot_id+1, sizeof(long))) == NULL){
		printf("malloc error for h_tbl->istack_hash\n");
		exit(0);
	};
	
	h_tbl->ntot_list = ((long) num_4_ele) * nele;
	if((h_tbl->id_hash = (long **) malloc(2*sizeof(long *))) == NULL) {
		printf("malloc error for h_tbl->id_hash \n");
		exit(0);
	}
	for(i=0;i<2;i++){
		if((h_tbl->id_hash[i] = (long *)calloc(h_tbl->ntot_list, sizeof(long))) == NULL){
			printf("malloc error for h_tbl->id_hash[%d]\n", i);
			exit(0);
		};
	};
	if((h_tbl->iflag_hash = (long *)calloc(h_tbl->ntot_list, sizeof(long))) == NULL){
		printf("malloc error for h_tbl->iflag_hash\n");
		exit(0);
	};
	
	return h_tbl;
};

void dealloc_sum_hash(struct sum_hash_tbl_c *h_tbl){
	free(h_tbl->id_hash[0]);
	free(h_tbl->id_hash[1]);
	free(h_tbl->id_hash);
	free(h_tbl->iflag_hash);
	
	free(h_tbl->istack_hash);
	
	free(h_tbl);
	return;
}

void clear_sum_hash(struct sum_hash_tbl_c *h_tbl){
	long i;
	
	h_tbl->istack_hash[0] = 0;
	for(i=0;i<h_tbl->ntot_id;i++){h_tbl->istack_hash[i+1] = 0;};
	for(i=0;i<h_tbl->ntot_list;i++){
		h_tbl->id_hash[0][i] = 0;
		h_tbl->id_hash[1][i] = 0;
		h_tbl->iflag_hash[i] = 0;
	};
	
	return;
};

void set_edge_hash_4_triangle(const int nedge_4_surf, const long numsurf,
							  const int nnod_4_surf, long **ie_viz, 
							  struct sum_hash_tbl_c *h_tbl){
	int is1, is2, k1;
	long isurf;
	long ihash, icou;
	
	long *num_edge_hash;
	if((num_edge_hash = (long *)calloc(h_tbl->ntot_id, sizeof(long))) == NULL){
		printf("malloc error for num_edge_hash\n");
		exit(0);
	};
	
	/* ! Count numbers */
	for(isurf=0;isurf<numsurf;isurf++){
		for(k1=0;k1<nedge_4_surf;k1++){
			is1 = h_tbl->node_on_edge_tri[k1][0];
			is2 = h_tbl->node_on_edge_tri[k1][1];
			ihash = ie_viz[isurf][is1] + ie_viz[isurf][is2];
			num_edge_hash[ihash] = num_edge_hash[ihash] + 1;
		};
	};
	
/* ! Set stacks */
    h_tbl->istack_hash[0] = 0;
	for(ihash=0;ihash<h_tbl->ntot_id;ihash++){h_tbl->istack_hash[ihash+1] = 0;};
	for(ihash=0;ihash<h_tbl->ntot_id;ihash++){
		h_tbl->istack_hash[ihash+1] = h_tbl->istack_hash[ihash] + num_edge_hash[ihash];
		if(h_tbl->istack_hash[ihash+1] <= (nedge_4_surf*numsurf)){
			h_tbl->iend_hash = ihash;
		};
	};
/* ! Set ID */
	for(ihash=0;ihash<h_tbl->ntot_id;ihash++){num_edge_hash[ihash] = 0;};
	for(isurf=0;isurf<numsurf;isurf++){
		for(k1=0;k1<nedge_4_surf;k1++){
			is1 = h_tbl->node_on_edge_tri[k1][0];
			is2 = h_tbl->node_on_edge_tri[k1][1];
			ihash = ie_viz[isurf][is1] + ie_viz[isurf][is2];
			num_edge_hash[ihash] = num_edge_hash[ihash] + 1;
			icou = h_tbl->istack_hash[ihash] + num_edge_hash[ihash];
			h_tbl->id_hash[0][icou-1] = isurf;
			h_tbl->id_hash[1][icou-1] = (long) k1;
		};
	};
	free(num_edge_hash);
	return;
};

void mark_all_edges_on_triangle(const int nnod_4_surf, long **ie_viz, 
							struct sum_hash_tbl_c *h_tbl){
	int is1, is2, js1, js2;
	long isurf, iedge, jsurf, jedge;
	long inod1, inod2, jnod1, jnod2;
	long ihash, ist, ied, k1, k2;
	
	for(ihash=0;ihash<h_tbl->ntot_list;ihash++){h_tbl->iflag_hash[ihash] = 0;};
	for(ihash=0;ihash<h_tbl->iend_hash;ihash++){
		ist = h_tbl->istack_hash[ihash];
		ied = h_tbl->istack_hash[ihash+1];
		if(ied == ist+1){
			h_tbl->iflag_hash[ist] = ist + 1;
		}else if(ied > ist){
			for(k1=ist;k1<ied;k1++){
				if(h_tbl->iflag_hash[k1] == 0){
					h_tbl->iflag_hash[k1] = k1 + 1;
					
					isurf = h_tbl->id_hash[0][k1];
					iedge = h_tbl->id_hash[1][k1];
					is1 = h_tbl->node_on_edge_tri[iedge][0];
					is2 = h_tbl->node_on_edge_tri[iedge][1];
					inod1 = ie_viz[isurf][is1];
					inod2 = ie_viz[isurf][is2];
					for(k2=k1+1;k2<ied;k2++){
						jsurf = h_tbl->id_hash[0][k2];
						jedge = h_tbl->id_hash[1][k2];
						js1 = h_tbl->node_on_edge_tri[jedge][0];
						js2 = h_tbl->node_on_edge_tri[jedge][1];
						jnod1 = ie_viz[jsurf][js1];
						jnod2 = ie_viz[jsurf][js2];
						if((inod2-inod1) == (jnod2-jnod1)){
							h_tbl->iflag_hash[k2] =   k1 + 1;
						}else if((inod2-inod1) == (jnod1-jnod2)){
							h_tbl->iflag_hash[k2] = -(k1 + 1);
						};
					};
				};
			};
		};
	};
	return;
}
