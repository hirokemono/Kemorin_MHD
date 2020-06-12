/*
 *  set_psf_viewer.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/13.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_psf_viewer.h"

static void set_viewer_udt_quad(struct psf_data *viz_copied, struct psf_data *viz_org){
	int i;

	for (i = 0; i < viz_org->nele_viz; i++) {
		viz_copied->ie_viz[2*i  ][0] = viz_org->ie_viz[i][0];
		viz_copied->ie_viz[2*i  ][1] = viz_org->ie_viz[i][1];
		viz_copied->ie_viz[2*i  ][2] = viz_org->ie_viz[i][2];
		
		viz_copied->ie_viz[2*i+1][0] = viz_org->ie_viz[i][2];
		viz_copied->ie_viz[2*i+1][1] = viz_org->ie_viz[i][3];
		viz_copied->ie_viz[2*i+1][2] = viz_org->ie_viz[i][0];
	};
	return;
}


static void count_new_node_for_mapping_tri(struct psf_data *viz_s, struct psf_data *viz_tmp){
	int num_map_patch;
	int inod, iele, k, nd;
	int num_add;
	long ie_1ele[5];
	double xyz_tri[9];
	
	num_add = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_1ele[0] = viz_tmp->ie_viz[iele][0];
		ie_1ele[1] = viz_tmp->ie_viz[iele][1];
		ie_1ele[2] = viz_tmp->ie_viz[iele][2];
		
		for (k=0; k<3; k++) {
			inod = ie_1ele[k]-1;
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xx_viz[inod][nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);
		
		num_add = num_add + num_map_patch-1;
	}
	
	viz_s->nnod_viz = num_add + viz_tmp->nnod_viz;
	viz_s->nele_viz = num_add + viz_tmp->nele_viz;
	viz_s->nnod_added_4_map = num_add;
	viz_s->nnod_4_ele_viz = ITHREE;
	return;
}

static void count_new_node_for_mapping_quad(struct psf_data *viz_s, struct psf_data *viz_tmp){
	int num_map_patch;
	int inod, iele, k, nd;
	int num_add;
	long ie_1ele[5];
	double xyz_tri[9];
	
	num_add = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_1ele[0] = viz_tmp->ie_viz[iele][0];
		ie_1ele[1] = viz_tmp->ie_viz[iele][1];
		ie_1ele[2] = viz_tmp->ie_viz[iele][2];
		
		for (k=0; k<3; k++) {
			inod = ie_1ele[k];
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xx_viz[inod][nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);		
		num_add = num_add + num_map_patch-1;
		
		ie_1ele[0] = viz_tmp->ie_viz[iele][2];
		ie_1ele[1] = viz_tmp->ie_viz[iele][3];
		ie_1ele[2] = viz_tmp->ie_viz[iele][0];
		for (k=0; k<3; k++) {
			inod = ie_1ele[k];
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xx_viz[inod][nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);
		num_add = num_add + num_map_patch-1;
	}
	
	viz_s->nnod_viz = num_add + viz_tmp->nnod_viz;
	viz_s->nele_viz = num_add + 2*viz_tmp->nele_viz;
	viz_s->nnod_added_4_map = num_add;
	viz_s->nnod_4_ele_viz = ITHREE;
	
	return;
}

static int cut_each_patch_for_map(int iele, int icou, int nnod_org, int iele_end, long ie_patch[5], 
								   struct psf_data *viz_s){
	int num_map_patch;
    long inod;
    int k, nd, i, j, i1, i2;
	long ie_cut[9], inod_src[4];
	double xyz_tri[9], coef_cut[4];
	
	for (k=0; k<3; k++) {
		inod = ie_patch[k];
		for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_s->xx_viz[inod-1][nd];};
	};
	ie_patch[3] = nnod_org+icou+1;
	ie_patch[4] = nnod_org+icou+2;
	
	num_map_patch = cut_new_patch_at_phi180(xyz_tri, ie_cut, inod_src, coef_cut);
	for (i=0; i<(num_map_patch-1); i++){
		i1 = inod_src[2*i  ]-1;
		i2 = inod_src[2*i+1]-1;
		viz_s->inod_org_4_map_itp[icou+i][0] = ie_patch[i1];
		viz_s->inod_org_4_map_itp[icou+i][1] = ie_patch[i2];
		viz_s->coef_4_map_itp[icou+i][0] = coef_cut[2*i  ];
		viz_s->coef_4_map_itp[icou+i][1] = coef_cut[2*i+1];
	};
	for (k=0; k<3; k++) {
		j = ie_cut[k]-1;
		viz_s->ie_viz[iele][k] = ie_patch[j];
	};
	for (i=1; i<(num_map_patch); i++) {
		for (k=0; k<3; k++) {
			j = ie_cut[3*i+k]-1;
			viz_s->ie_viz[iele_end+i-1][k] = ie_patch[j];
		};
	};

	return num_map_patch;
}

static void cut_patches_for_map_tri(int nele_org, struct psf_data *viz_s){
	int num_map_patch;
	int iele, icou;
	int nnod_org, iele_end;
	long ie_patch[5];
	
	nnod_org = viz_s->nnod_viz - viz_s->nnod_added_4_map;
	iele_end = nele_org;
	icou = 0;
	for(iele=0; iele<nele_org; iele++){
		ie_patch[0] = viz_s->ie_viz[iele][0];
		ie_patch[1] = viz_s->ie_viz[iele][1];
		ie_patch[2] = viz_s->ie_viz[iele][2];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch, viz_s);
		
		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
	}
	
	return;
}

static void cut_patches_for_map_quad(int nele_org, struct psf_data *viz_s){
	int num_map_patch;
	int iele, icou;
	int nnod_org, iele_end;
	long ie_patch[5];
	
	nnod_org = viz_s->nnod_viz - viz_s->nnod_added_4_map;
	iele_end = 2*nele_org;
	icou = 0;
	for(iele=0; iele<nele_org; iele++){
		ie_patch[0] = viz_s->ie_viz[iele][0];
		ie_patch[1] = viz_s->ie_viz[iele][1];
		ie_patch[2] = viz_s->ie_viz[iele][2];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch, viz_s);
		
		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
		
		
		ie_patch[0] = viz_s->ie_viz[iele][2];
		ie_patch[1] = viz_s->ie_viz[iele][3];
		ie_patch[2] = viz_s->ie_viz[iele][0];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch, viz_s);
		
		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
	}
	
	return;
}

static void set_new_node_for_mapping(struct psf_data *viz_s){
	int i, i1, i2;
	int nnod_org;
	
	nnod_org = viz_s->nnod_viz - viz_s->nnod_added_4_map;
	for(i=0; i<viz_s->nnod_added_4_map; i++){
		i1 = viz_s->inod_org_4_map_itp[i][0]-1;
		i2 = viz_s->inod_org_4_map_itp[i][1]-1;

		viz_s->xx_viz[nnod_org+i][0]
			= viz_s->coef_4_map_itp[i][0] * viz_s->xx_viz[i1][0]
			+ viz_s->coef_4_map_itp[i][1] * viz_s->xx_viz[i2][0];
		viz_s->xx_viz[nnod_org+i][1] = ZERO;
		viz_s->xx_viz[nnod_org+i][2]
			= viz_s->coef_4_map_itp[i][0] * viz_s->xx_viz[i1][2]
			+ viz_s->coef_4_map_itp[i][1] * viz_s->xx_viz[i2][2];
	};

	return;
};

static void set_new_data_for_mapping(struct psf_data *viz_s){
	int i, i1, i2, nd;
	int ncomp, nnod_org;
	
	ncomp =    viz_s->ncomptot;
	nnod_org = viz_s->nnod_viz - viz_s->nnod_added_4_map;
	for(i=0; i<viz_s->nnod_added_4_map; i++){
		i1 = viz_s->inod_org_4_map_itp[i][0]-1;
		i2 = viz_s->inod_org_4_map_itp[i][1]-1;
		for (nd=0; nd<ncomp; nd++) {
			viz_s->d_nod[nnod_org+i][nd]
			= viz_s->coef_4_map_itp[i][0] * viz_s->d_nod[i1][nd]
			+ viz_s->coef_4_map_itp[i][1] * viz_s->d_nod[i2][nd];
		};
	};
	return;
};


void set_viewer_ucd_data(struct psf_data *viz_s, struct psf_data *viz_tmp){
	
	viz_s->nfield = viz_tmp->nfield;
	alloc_psf_field_name_c(viz_s);
    copy_viewer_udt_field_name(viz_s, viz_tmp);

	viz_s->nnod_viz = viz_tmp->nnod_viz;
	if (viz_tmp->nnod_4_ele_viz == 4) {
		viz_s->nele_viz = 2*viz_tmp->nele_viz;
		viz_s->nnod_4_ele_viz = ITHREE;
	} else {
		viz_s->nele_viz = viz_tmp->nele_viz;
		viz_s->nnod_4_ele_viz = viz_tmp->nnod_4_ele_viz;
	};
		
	alloc_viz_node_s(viz_s);
	alloc_viz_ele_s(viz_s);
    alloc_psf_field_data_c(viz_s);
	alloc_psf_data_s(viz_s);

	copy_viewer_udt_node(viz_s, viz_tmp);
	copy_viewer_udt_data(viz_s, viz_tmp);

	if (viz_tmp->nnod_4_ele_viz == 4) {
		set_viewer_udt_quad(viz_s, viz_tmp);
	} else {
		copy_viewer_udt_connect(viz_s, viz_tmp);
	};

	dealloc_psf_data_s(viz_tmp);
	dealloc_psf_mesh_c(viz_tmp);
}

void set_evolution_udt_data(struct psf_data *viz_s, struct psf_data *viz_tmp){
	
	copy_viewer_udt_data(viz_s, viz_tmp);
	dealloc_psf_data_s(viz_tmp);
}

void set_ucd_with_mapping(struct psf_data *viz_s, struct psf_data *viz_tmp){
	
	viz_s->nfield = viz_tmp->nfield;
	alloc_psf_field_name_c(viz_s);
	copy_viewer_udt_field_name(viz_s, viz_tmp);

	if (viz_tmp->nnod_4_ele_viz == 4) {
		count_new_node_for_mapping_quad(viz_s, viz_tmp);
	} else {
		count_new_node_for_mapping_tri(viz_s, viz_tmp);
	};

	alloc_psf_data_s(viz_s);
	alloc_viz_ele_s(viz_s);
    alloc_psf_field_data_c(viz_s);
	alloc_viz_node_s(viz_s);
	alloc_psf_cutting_4_map(viz_s);
	
	copy_viewer_udt_node(viz_s, viz_tmp);
	copy_viewer_udt_data(viz_s, viz_tmp);
	if (viz_tmp->nnod_4_ele_viz == 4) {
		set_viewer_udt_quad(viz_s, viz_tmp);
		cut_patches_for_map_quad(viz_tmp->nele_viz, viz_s);
	} else {
		copy_viewer_udt_connect(viz_s, viz_tmp);
		cut_patches_for_map_tri(viz_tmp->nele_viz, viz_s);
	};
	set_new_node_for_mapping(viz_s);
	set_new_data_for_mapping(viz_s);

	dealloc_psf_data_s(viz_tmp);
	dealloc_psf_mesh_c(viz_tmp);
	return;
}

void set_evolution_map_udt_data(struct psf_data *viz_s, struct psf_data *viz_tmp){
	
	copy_viewer_udt_data(viz_s, viz_tmp);
	set_new_data_for_mapping(viz_s);
	dealloc_psf_data_s(viz_tmp);
}

