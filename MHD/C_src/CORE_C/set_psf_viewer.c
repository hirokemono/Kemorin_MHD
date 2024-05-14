/*
 *  set_psf_viewer.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/13.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_psf_viewer.h"

static struct map_interpolate * alloc_psf_cutting_4_map(void){
    struct map_interpolate *map_itp = (struct map_interpolate *) malloc(sizeof(struct map_interpolate));
    if(map_itp == NULL){
        printf("malloc error for map_itp \n");
        exit(0);
    }
    return map_itp;
};

static void alloc_psf_cutting_4_map_item(struct map_interpolate *map_itp){
    map_itp->inod_org_4_map_itp = (long *)malloc(2*map_itp->nnod_added_4_map*sizeof(long));
    if(map_itp->inod_org_4_map_itp  == NULL){
        printf("malloc error for map_itp->inod_org_4_map_itp \n");
        exit(0);
    }
    map_itp->coef_4_map_itp = (double *)malloc(2*map_itp->nnod_added_4_map*sizeof(double));
    if(map_itp->coef_4_map_itp  == NULL){
        printf("malloc error for map_itp->coef_4_map_itp \n");
        exit(0);
    }
    return;
};

static void dealloc_psf_cutting_4_map(struct map_interpolate *map_itp){
free(map_itp->inod_org_4_map_itp);
free(map_itp->coef_4_map_itp);
free(map_itp);
return;
};


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


static void count_new_node_for_mapping_tri(struct psf_data *viz_s,
                                           struct psf_data *viz_tmp,
                                           struct map_interpolate *map_itp){
	int num_map_patch;
	int iele, k, nd;
    long inod;
	int num_add;
	long ie_1ele[5];
	double xyz_tri[9];
	
	num_add = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_1ele[0] = viz_tmp->ie_viz[iele][0];
		ie_1ele[1] = viz_tmp->ie_viz[iele][1];
		ie_1ele[2] = viz_tmp->ie_viz[iele][2];
		
		for (k=0; k<3; k++) {
			inod = ie_1ele[k] - 1;
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);
		
		num_add = num_add + num_map_patch-1;
	}
	
	viz_s->nnod_viz = num_add + viz_tmp->nnod_viz;
	viz_s->nele_viz = num_add + viz_tmp->nele_viz;
	viz_s->nnod_4_ele_viz = ITHREE;
    map_itp->nnod_added_4_map = num_add;
	return;
}

static void count_new_node_for_mapping_quad(struct psf_data *viz_s,
                                            struct psf_data *viz_tmp,
                                            struct map_interpolate *map_itp){
	int num_map_patch;
    long inod;
	int iele, k, nd;
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
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);		
		num_add = num_add + num_map_patch-1;
		
		ie_1ele[0] = viz_tmp->ie_viz[iele][2];
		ie_1ele[1] = viz_tmp->ie_viz[iele][3];
		ie_1ele[2] = viz_tmp->ie_viz[iele][0];
		for (k=0; k<3; k++) {
			inod = ie_1ele[k];
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
		};
		num_map_patch = count_new_patch_at_phi180(&xyz_tri[0]);
		num_add = num_add + num_map_patch-1;
	}
	
	viz_s->nnod_viz = num_add + viz_tmp->nnod_viz;
	viz_s->nele_viz = num_add + 2*viz_tmp->nele_viz;
	viz_s->nnod_4_ele_viz = ITHREE;
    map_itp->nnod_added_4_map = num_add;
	return;
}

static int cut_each_patch_for_map(int iele, int icou, long nnod_org, long iele_end, long ie_patch[5],
                                  struct psf_data *viz_s, struct map_interpolate *map_itp){
	int num_map_patch;
    long inod, i1, i2;
    long j;
    int k, nd, i;
	long ie_cut[9], inod_src[4];
	double xyz_tri[9], coef_cut[4];
	
	for (k=0; k<3; k++) {
		inod = ie_patch[k];
		for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_s->xyzw_viz[(inod-1)*IFOUR + nd];};
	};
	ie_patch[3] = nnod_org+icou+1;
	ie_patch[4] = nnod_org+icou+2;
	
	num_map_patch = cut_new_patch_at_phi180(xyz_tri, ie_cut, inod_src, coef_cut);
	for (i=0; i<(num_map_patch-1); i++){
		i1 = inod_src[2*i  ]-1;
		i2 = inod_src[2*i+1]-1;
        map_itp->inod_org_4_map_itp[2*(icou+i)  ] = ie_patch[i1];
        map_itp->inod_org_4_map_itp[2*(icou+i)+1] = ie_patch[i2];
        map_itp->coef_4_map_itp[2*(icou+i)  ] = coef_cut[2*i  ];
        map_itp->coef_4_map_itp[2*(icou+i)+1] = coef_cut[2*i+1];
	};
	for (k=0; k<3; k++) {
		j = ie_cut[k] - 1;
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

static void cut_patches_for_map_tri(long nele_org, struct psf_data *viz_s,
                                    struct map_interpolate *map_itp){
	int num_map_patch;
	int iele, icou;
    long nnod_org, iele_end;
	long ie_patch[5];
	
	nnod_org = viz_s->nnod_viz - map_itp->nnod_added_4_map;
	iele_end = nele_org;
	icou = 0;
	for(iele=0; iele<nele_org; iele++){
		ie_patch[0] = viz_s->ie_viz[iele][0];
		ie_patch[1] = viz_s->ie_viz[iele][1];
		ie_patch[2] = viz_s->ie_viz[iele][2];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch,
                                               viz_s, map_itp);
		
		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
	}
	
	return;
}

static void cut_patches_for_map_quad(long nele_org, struct psf_data *viz_s,
                                     struct map_interpolate *map_itp){
	int num_map_patch;
	int iele, icou;
    long nnod_org, iele_end;
	long ie_patch[5];
	
	nnod_org = viz_s->nnod_viz - map_itp->nnod_added_4_map;
	iele_end = 2*nele_org;
	icou = 0;
	for(iele=0; iele<nele_org; iele++){
		ie_patch[0] = viz_s->ie_viz[iele][0];
		ie_patch[1] = viz_s->ie_viz[iele][1];
		ie_patch[2] = viz_s->ie_viz[iele][2];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch,
                                               viz_s, map_itp);

		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
		
		
		ie_patch[0] = viz_s->ie_viz[iele][2];
		ie_patch[1] = viz_s->ie_viz[iele][3];
		ie_patch[2] = viz_s->ie_viz[iele][0];
		
		num_map_patch = cut_each_patch_for_map(iele, icou, nnod_org, iele_end, ie_patch,
                                               viz_s, map_itp);

		icou = icou + num_map_patch-1;
		iele_end = iele_end + num_map_patch-1;
	}
	
	return;
}

static void set_new_node_for_mapping(struct map_interpolate *map_itp, struct psf_data *viz_s){
    int i;
    long i1, i2;
	long nnod_org;
	
	nnod_org = viz_s->nnod_viz - map_itp->nnod_added_4_map;
	for(i=0; i<map_itp->nnod_added_4_map; i++){
		i1 = map_itp->inod_org_4_map_itp[2*i  ]-1;
		i2 = map_itp->inod_org_4_map_itp[2*i+1]-1;

		viz_s->xyzw_viz[(nnod_org+i)*IFOUR + 0]
			= map_itp->coef_4_map_itp[2*i  ] * viz_s->xyzw_viz[i1*IFOUR + 0]
			+ map_itp->coef_4_map_itp[2*i+1] * viz_s->xyzw_viz[i2*IFOUR + 0];
		viz_s->xyzw_viz[(nnod_org+i)*IFOUR + 1] = ZERO;
		viz_s->xyzw_viz[(nnod_org+i)*IFOUR + 2]
			= map_itp->coef_4_map_itp[2*i  ] * viz_s->xyzw_viz[i1*IFOUR + 2]
			+ map_itp->coef_4_map_itp[2*i+1] * viz_s->xyzw_viz[i2*IFOUR + 2];
	};

	return;
};

static void set_new_data_for_mapping(struct map_interpolate *map_itp, struct psf_data *viz_s){
	int i, nd;
    long i1, i2;
	long ncomp, nnod_org;
	
	ncomp =    viz_s->ncomptot;
	nnod_org = viz_s->nnod_viz - map_itp->nnod_added_4_map;
	for(i=0; i<map_itp->nnod_added_4_map; i++){
		i1 = map_itp->inod_org_4_map_itp[2*i  ]-1;
		i2 = map_itp->inod_org_4_map_itp[2*i+1]-1;
		for (nd=0; nd<ncomp; nd++) {
			viz_s->d_nod[(nnod_org+i)*viz_s->ncomptot + nd]
			= map_itp->coef_4_map_itp[2*i  ] * viz_s->d_nod[i1*viz_s->ncomptot + nd]
             + map_itp->coef_4_map_itp[2*i+1] * viz_s->d_nod[i2*viz_s->ncomptot + nd];
		};
	};
	return;
};


void set_viewer_fieldline_data(struct fline_data *fline_d,
                               struct psf_data *viz_tmp){
	alloc_fline_field_name_c(viz_tmp->nfield, fline_d);
    fline_d->ncomptot = copy_viewer_udt_field_name(viz_tmp, fline_d->nfield,
                                                   fline_d->ncomp,  fline_d->istack_comp,
                                                   fline_d->id_coord, fline_d->data_name);

    alloc_fline_node_s(viz_tmp->nnod_viz, fline_d);
    alloc_fline_ele_s(viz_tmp->nele_viz, viz_tmp->nnod_4_ele_viz, fline_d);

    alloc_fline_field_data_c(fline_d);

	copy_viewer_udt_node(viz_tmp, fline_d->inod_fline, fline_d->xyzw_fline);
	copy_viewer_udt_data(viz_tmp, fline_d->nnod_fline, fline_d->ncomptot, fline_d->d_nod);

    copy_viewer_udt_connect(viz_tmp, fline_d->iedge_fline);

	dealloc_psf_data_s(viz_tmp);
	dealloc_psf_mesh_c(viz_tmp);
    return;
}

void set_viewer_data_with_mapping(struct psf_data *viz_s, struct psf_data *viz_tmp){
	
	viz_s->nfield = viz_tmp->nfield;
	alloc_psf_field_name_c(viz_s);
    viz_s->ncomptot = copy_viewer_udt_field_name(viz_tmp, viz_s->nfield,
                                                 viz_s->ncomp,  viz_s->istack_comp,
                                                 viz_s->id_coord, viz_s->data_name);

    struct map_interpolate *map_itp = alloc_psf_cutting_4_map();
	if (viz_tmp->nnod_4_ele_viz == 4) {
		count_new_node_for_mapping_quad(viz_s, viz_tmp, map_itp);
	} else {
		count_new_node_for_mapping_tri(viz_s, viz_tmp, map_itp);
	};

	alloc_psf_data_s(viz_s);
	alloc_viz_ele_s(viz_s);
    alloc_psf_field_data_c(viz_s);
	alloc_viz_node_s(viz_s);
    
    alloc_psf_cutting_4_map_item(map_itp);
	
	copy_viewer_udt_node(viz_tmp, viz_s->inod_viz, viz_s->xyzw_viz);
	copy_viewer_udt_data(viz_tmp, viz_s->nnod_viz, viz_s->ncomptot,
                         viz_s->d_nod);
	if (viz_tmp->nnod_4_ele_viz == 4) {
		set_viewer_udt_quad(viz_s, viz_tmp);
		cut_patches_for_map_quad(viz_tmp->nele_viz, viz_s, map_itp);
	} else {
        copy_viewer_udt_connect(viz_tmp, viz_s->ie_viz);
		cut_patches_for_map_tri(viz_tmp->nele_viz, viz_s, map_itp);
	};
	set_new_node_for_mapping(map_itp, viz_s);
	set_new_data_for_mapping(map_itp, viz_s);
    dealloc_psf_cutting_4_map(map_itp);

	dealloc_psf_data_s(viz_tmp);
	dealloc_psf_mesh_c(viz_tmp);
	return;
}
