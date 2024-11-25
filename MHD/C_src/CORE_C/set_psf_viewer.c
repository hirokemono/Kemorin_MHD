/*
 *  set_psf_viewer.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/13.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_psf_viewer.h"

struct map_interpolate * alloc_psf_cutting_4_map(void){
    struct map_interpolate *map_itp = (struct map_interpolate *) malloc(sizeof(struct map_interpolate));
    if(map_itp == NULL){
        printf("malloc error for map_itp \n");
        exit(0);
    }
    return map_itp;
};

void alloc_psf_cutting_4_map_item(struct map_interpolate *map_itp){
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

void dealloc_psf_cutting_4_map(struct map_interpolate *map_itp){
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

int kcou, lcou;
static void count_new_node_for_mapping_tri(struct psf_data *viz_s,
                                           struct psf_data *viz_tmp,
                                           struct map_interpolate *map_itp){
	int iflag_add;
	int iele, k, nd;
    long inod;
	long ie_1ele[5];
	double xyz_tri[9];
	
    kcou = 0;
    int nnod_add = 0;
    int nele_add = 0;
    for(iele=0; iele<viz_tmp->nele_viz; iele++){
        ie_1ele[0] = viz_tmp->ie_viz[iele][0];
        ie_1ele[1] = viz_tmp->ie_viz[iele][1];
        ie_1ele[2] = viz_tmp->ie_viz[iele][2];
        
        for (k=0; k<3; k++) {
            inod = ie_1ele[k] - 1;
            for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
        };
        iflag_add = count_new_patch_at_phi180(&xyz_tri[0]);

        nele_add = nele_add + iflag_add / 100;
        nnod_add = nnod_add + (iflag_add/10) % 10;
        if(iflag_add > 0) kcou = kcou + 1;
    }
	
    map_itp->nnod_added_4_map = nnod_add;
    map_itp->nele_added_4_map = nele_add;
	return;
}

static void count_new_node_for_mapping_quad(struct psf_data *viz_s,
                                            struct psf_data *viz_tmp,
                                            struct map_interpolate *map_itp){
	int iflag_add;
    long inod;
	int iele, k, nd;
	long ie_1ele[5];
	double xyz_tri[9];
	
    int nnod_add = 0;
    int nele_add = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_1ele[0] = viz_tmp->ie_viz[iele][0];
		ie_1ele[1] = viz_tmp->ie_viz[iele][1];
		ie_1ele[2] = viz_tmp->ie_viz[iele][2];
		
		for (k=0; k<3; k++) {
			inod = ie_1ele[k] - 1;
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
		};
        iflag_add = count_new_patch_at_phi180(&xyz_tri[0]);
        nele_add = nele_add + iflag_add / 100;
		nnod_add = nnod_add + (iflag_add/10) % 10;
        
		ie_1ele[0] = viz_tmp->ie_viz[iele][2];
		ie_1ele[1] = viz_tmp->ie_viz[iele][3];
		ie_1ele[2] = viz_tmp->ie_viz[iele][0];
		for (k=0; k<3; k++) {
			inod = ie_1ele[k] - 1;
			for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = viz_tmp->xyzw_viz[inod*IFOUR + nd];};
		};
        iflag_add = count_new_patch_at_phi180(&xyz_tri[0]);
        nele_add = nele_add + iflag_add / 100;
        nnod_add = nnod_add + (iflag_add/10) % 10;
	}
    
    map_itp->nnod_added_4_map = nnod_add;
    map_itp->nele_added_4_map = nele_add;
	return;
}

static int cut_each_patch_for_map(int iele, int icou,  long iele_end,
                                  long ie_patch[7], double *xyzw_org,
                                  struct psf_data *viz_s, struct map_interpolate *map_itp){
	int iflag_add = 0;
    long inod, i1, i2;
    long j;
    int k, nd, i;
    long ie[3];
	long ie_cut[9], inod_src[4];
	double xyz_tri[9], coef_cut[4];
    double  y_cut[3];
	
	for (k=0; k<3; k++) {
        ie[k] = ie_patch[k];
		inod = ie_patch[k] - 1;
		for (nd=0; nd<3; nd++) {xyz_tri[3*k+nd] = xyzw_org[inod*IFOUR + nd];};
	};
	ie_patch[3] = 1+icou + map_itp->nnod_org;
	ie_patch[4] = 2+icou + map_itp->nnod_org;
    ie_patch[5] = 1+icou + map_itp->nnod_org + map_itp->nnod_added_4_map;
    ie_patch[6] = 2+icou + map_itp->nnod_org + map_itp->nnod_added_4_map;

    
    iflag_add = cut_new_patch_at_phi180(xyz_tri, ie_cut, y_cut, inod_src, coef_cut);

    if(iflag_add == 0){
        for (k=0; k<3; k++){viz_s->ie_viz[iele][k] = ie_patch[k];};
        return 0;
    }

    int nele_add = iflag_add / 100;
    int nnod_add = (iflag_add/10) % 10;


    for (i=0; i<nnod_add; i++){
        i1 = inod_src[2*i  ]-1;
        i2 = inod_src[2*i+1]-1;
        map_itp->inod_org_4_map_itp[2*(icou+i)  ] = ie_patch[i1];
        map_itp->inod_org_4_map_itp[2*(icou+i)+1] = ie_patch[i2];
        map_itp->coef_4_map_itp[2*(icou+i)  ] = coef_cut[2*i  ];
        map_itp->coef_4_map_itp[2*(icou+i)+1] = coef_cut[2*i+1];
    };

    for (k=0; k<3; k++) {
		j = ie_cut[k] - 1;
        if(y_cut[0] < 0.0 && j >2){j = j + 2;};
		viz_s->ie_viz[iele][k] = ie_patch[j];
	};
	for(i=0; i<nele_add; i++) {
		for (k=0; k<3; k++) {
			j = ie_cut[3*(i+1)+k]-1;
            if(y_cut[i+1] < 0.0 && j >2){j = j + 2;};
			viz_s->ie_viz[iele_end+i][k] = ie_patch[j];
		};
	};
    return iflag_add;
}

static void cut_patches_for_map_tri(struct psf_data *viz_tmp, struct psf_data *viz_s,
                                    struct map_interpolate *map_itp){
	int iflag_add = 0;
	int iele, icou;
	long ie_patch[7];
	
	long iele_end = viz_tmp->nele_viz;
	int icou_nod = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_patch[0] = viz_tmp->ie_viz[iele][0];
		ie_patch[1] = viz_tmp->ie_viz[iele][1];
		ie_patch[2] = viz_tmp->ie_viz[iele][2];
		
        iflag_add = cut_each_patch_for_map(iele, icou_nod, iele_end, ie_patch,
                                           viz_tmp->xyzw_viz, viz_s, map_itp);
        iele_end = iele_end + iflag_add / 100;
        icou_nod = icou_nod + (iflag_add/10) % 10;
        if(iflag_add > 0) lcou = lcou + 1;

	}
    return;
}

static void cut_patches_for_map_quad(struct psf_data *viz_tmp, struct psf_data *viz_s,
                                     struct map_interpolate *map_itp){
	int iflag_add;
	int iele;
    long iele_end;
	long ie_patch[7];
	
	iele_end = viz_tmp->nele_viz;
    int icou_nod = 0;
	for(iele=0; iele<viz_tmp->nele_viz; iele++){
		ie_patch[0] = viz_s->ie_viz[iele][0];
		ie_patch[1] = viz_s->ie_viz[iele][1];
		ie_patch[2] = viz_s->ie_viz[iele][2];
		
        iflag_add = cut_each_patch_for_map(iele, icou_nod, iele_end, ie_patch,
                                           viz_tmp->xyzw_viz, viz_s, map_itp);
        iele_end = iele_end + iflag_add / 100;
        icou_nod = icou_nod + (iflag_add/10) % 10;

		ie_patch[0] = viz_s->ie_viz[iele][2];
		ie_patch[1] = viz_s->ie_viz[iele][3];
		ie_patch[2] = viz_s->ie_viz[iele][0];
		
        iflag_add = cut_each_patch_for_map(iele, icou_nod, iele_end, ie_patch,
                                           viz_tmp->xyzw_viz, viz_s, map_itp);
        iele_end = iele_end + iflag_add / 100;
        icou_nod = icou_nod + (iflag_add/10) % 10;
	}
	
	return;
}

static void set_new_data_for_mapping(struct map_interpolate *map_itp,
                                     long ncomptot, double *d_nod, double *d_itp){
    long i, nd;
    long i1, i2;
	
	for(i=0; i<map_itp->nnod_added_4_map; i++){
		i1 = map_itp->inod_org_4_map_itp[2*i  ]-1;
		i2 = map_itp->inod_org_4_map_itp[2*i+1]-1;
		for (nd=0; nd<ncomptot; nd++) {
            d_itp[i*ncomptot + nd] = map_itp->coef_4_map_itp[2*i  ] * d_nod[i1*ncomptot + nd]
                                    + map_itp->coef_4_map_itp[2*i+1] * d_nod[i2*ncomptot + nd];
            if(d_itp[i*ncomptot + nd]*d_itp[i*ncomptot + nd] < 1.0e-24){
                d_itp[i*ncomptot + nd] = 0.0;
            }
		};
	};
	return;
};


void set_viewer_points_data(struct psf_data *points_d,
                            struct psf_data *viz_tmp){
    points_d->nfield = viz_tmp->nfield;
    alloc_psf_field_name_c(points_d);
    points_d->ncomptot = copy_viewer_udt_field_name(viz_tmp, points_d->nfield,
                                                    points_d->ncomp,  points_d->istack_comp,
                                                    points_d->id_coord, points_d->data_name);

    points_d->nnod_viz = viz_tmp->nnod_viz;
    alloc_viz_node_s(points_d);
    alloc_psf_field_data_c(points_d);

	copy_viewer_udt_node(viz_tmp, points_d->inod_viz, points_d->xyzw_viz);
	copy_viewer_udt_data(viz_tmp, points_d->nnod_viz, points_d->ncomptot, points_d->d_nod);

    dealloc_psf_field_data_c(viz_tmp);
	dealloc_psf_mesh_c(viz_tmp);
    return;
}

void set_viewer_fieldline_data(struct psf_data *fline_d,
                               struct psf_data *viz_tmp){
    fline_d->nfield = viz_tmp->nfield;
    alloc_psf_field_name_c(fline_d);
    fline_d->ncomptot = copy_viewer_udt_field_name(viz_tmp, fline_d->nfield,
                                                   fline_d->ncomp,  fline_d->istack_comp,
                                                   fline_d->id_coord, fline_d->data_name);

    fline_d->nnod_viz =       viz_tmp->nnod_viz;
    fline_d->nele_viz =       viz_tmp->nele_viz;
    fline_d->nnod_4_ele_viz = viz_tmp->nnod_4_ele_viz;
    alloc_viz_node_s(fline_d);
    alloc_viz_ele_s(fline_d);

    alloc_psf_field_data_c(fline_d);

	copy_viewer_udt_node(viz_tmp, fline_d->inod_viz, fline_d->xyzw_viz);
	copy_viewer_udt_data(viz_tmp, fline_d->nnod_viz, fline_d->ncomptot, fline_d->d_nod);

    copy_viewer_udt_connect(viz_tmp, fline_d->ie_viz);

	dealloc_psf_mesh_c(viz_tmp);
    return;
}

long set_viewer_mesh_with_mapping(struct map_interpolate *map_itp,
                                  struct psf_data *viz_s,
                                  struct psf_data *viz_tmp){
//    shift_longitude(0.00, viz_tmp->nnod_viz, viz_tmp->xyzw_viz);

	viz_s->nfield = viz_tmp->nfield;
	alloc_psf_field_name_c(viz_s);
    viz_s->ncomptot = copy_viewer_udt_field_name(viz_tmp, viz_s->nfield,
                                                 viz_s->ncomp,  viz_s->istack_comp,
                                                 viz_s->id_coord, viz_s->data_name);

    map_itp->nnod_org = viz_tmp->nnod_viz;
	if (viz_tmp->nnod_4_ele_viz == 4) {
		count_new_node_for_mapping_quad(viz_s, viz_tmp, map_itp);
        viz_s->nele_viz = map_itp->nele_added_4_map + viz_tmp->nele_viz;
	} else {
		count_new_node_for_mapping_tri(viz_s, viz_tmp, map_itp);
        viz_s->nele_viz = map_itp->nele_added_4_map + viz_tmp->nele_viz;
	};
    viz_s->nnod_4_ele_viz = ITHREE;
    viz_s->nnod_viz = ITWO * map_itp->nnod_added_4_map + map_itp->nnod_org;

	alloc_viz_ele_s(viz_s);
    alloc_psf_field_data_c(viz_s);
	alloc_viz_node_s(viz_s);
    alloc_psf_data_s(viz_s);

    alloc_psf_cutting_4_map_item(map_itp);
	
	copy_viewer_udt_node(viz_tmp, viz_s->inod_viz, viz_s->xyzw_viz);
	if (viz_tmp->nnod_4_ele_viz == 4) {
		set_viewer_udt_quad(viz_s, viz_tmp);
		cut_patches_for_map_quad(viz_tmp, viz_s, map_itp);
	} else {
        copy_viewer_udt_connect(viz_tmp, viz_s->ie_viz);
		cut_patches_for_map_tri(viz_tmp, viz_s, map_itp);
	};
    
    long ist;
    ist = viz_s->nnod_viz - 2 * map_itp->nnod_added_4_map;
    set_new_data_for_mapping(map_itp, IFOUR,
                             &viz_s->xyzw_viz[0], &viz_s->xyzw_viz[IFOUR*ist]);

    ist = viz_s->nnod_viz - map_itp->nnod_added_4_map;
    set_new_data_for_mapping(map_itp, IFOUR,
                             &viz_s->xyzw_viz[0], &viz_s->xyzw_viz[IFOUR*ist]);

    long nadded_for_phi0 = map_itp->nnod_added_4_map;
	dealloc_psf_mesh_c(viz_tmp);
	return nadded_for_phi0;
}

void set_viewer_data_with_mapping(struct map_interpolate *map_itp,
                                  struct psf_data *viz_tmp, struct psf_data *viz_s){
    copy_viewer_udt_data(viz_tmp, viz_s->nnod_viz, viz_s->ncomptot,
                         viz_s->d_nod);
    long ist = viz_s->nnod_viz - 2 * map_itp->nnod_added_4_map;
    set_new_data_for_mapping(map_itp, viz_s->ncomptot,
                             &viz_s->d_nod[0], &viz_s->d_nod[viz_s->ncomptot*ist]);
    ist = viz_s->nnod_viz - map_itp->nnod_added_4_map;
    set_new_data_for_mapping(map_itp, viz_s->ncomptot,
                             &viz_s->d_nod[0], &viz_s->d_nod[viz_s->ncomptot*ist]);
    return;
}
