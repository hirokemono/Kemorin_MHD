/*
 *  set_normal_on_node_4_mesh.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/21.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include <math.h>
#include "set_normal_on_node_4_mesh.h"


static void set_domain_normal_4_each_node(struct viewer_mesh *mesh_s,
                                          double *normal_domain,
                                          double *norm_nod_domain,
                                          double *dist_nod_domain){
	int i, iele, idir;
    for (i=0; i<mesh_s->nsurf_domain_sf; i++) {
        iele = abs(mesh_s->isurf_domain_sf[i]) - 1;
        idir = abs(mesh_s->isurf_domain_sf[i]) / mesh_s->isurf_domain_sf[i];
        set_normal_4_each_node(i, iele, idir, mesh_s,
                               normal_domain, norm_nod_domain,
                               dist_nod_domain);
	};
	return;
}

static void set_ele_group_normal_4_each_node(struct viewer_mesh *mesh_s,
                                             double *normal_ele_grp,
                                             double *norm_nod_ele_grp,
                                             double *dist_nod_ele_grp){
	int i, iele, idir;
    for (i=0; i<mesh_s->nele_ele_sf; i++) {
        iele = abs(mesh_s->ele_item_sf[i]) - 1;
        idir = abs(mesh_s->ele_item_sf[i]) / mesh_s->ele_item_sf[i];
        set_normal_4_each_node(i, iele, idir, mesh_s,
                               normal_ele_grp, norm_nod_ele_grp,
                               dist_nod_ele_grp);
    }
    return;
}


static  void set_surf_group_normal_4_each_node(struct viewer_mesh *mesh_s,
                                               double *normal_surf_grp,
                                               double *norm_nod_surf_grp,
                                               double *dist_nod_surf_grp){
 	int i, iele, idir;
    for (i=0; i<mesh_s->nsurf_surf_sf; i++) {
        iele = abs(mesh_s->surf_item_sf[i]) - 1;
        idir = abs(mesh_s->surf_item_sf[i]) / mesh_s->surf_item_sf[i];
        set_normal_4_each_node(i, iele, idir, mesh_s,
                               normal_surf_grp, norm_nod_surf_grp,
                               dist_nod_surf_grp);
    }
    return;
}


static void set_normal_on_domain_group(struct viewer_mesh *mesh_s,
                                       struct normal_nod_work *wk_norm){
    int ip;
    long ist = mesh_s->ist_domain_patch * mesh_s->nsurf_each_tri;
    set_domain_normal_4_each_node(mesh_s,
                                  &mesh_s->normal_mesh_patch[4*ist],
                                  &mesh_s->normal_nod_mesh_patch[12*ist],
                                  wk_norm->dist_nod_domain);
    
    for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
        refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
                                    mesh_s->isurf_stack_domain_sf[ip],
                                    mesh_s->isurf_stack_domain_sf[ip+1],
                                    mesh_s->isurf_domain_sf, wk_norm->dist_nod_domain,
                                    &mesh_s->normal_nod_mesh_patch[12*ist],
                                    wk_norm);
    }
    return;
}

static void set_normal_on_node_element_group(struct viewer_mesh *mesh_s,
                                             struct normal_nod_work *wk_norm){
    int i, ip, ip_st;
    long ist = mesh_s->ist_ele_grp_patch * mesh_s->nsurf_each_tri;
    set_ele_group_normal_4_each_node(mesh_s,
                                     &mesh_s->normal_mesh_patch[4*ist],
                                     &mesh_s->normal_nod_mesh_patch[12*ist],
                                     wk_norm->dist_nod_ele_grp);

    for (i = 0; i < mesh_s->ngrp_ele_sf; i++){
        for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
            ip_st = ip + i * mesh_s->num_pe_sf;
            refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
                                        mesh_s->ele_stack_sf[ip_st], mesh_s->ele_stack_sf[ip_st+1],
                                        mesh_s->ele_item_sf, wk_norm->dist_nod_ele_grp,
                                        &mesh_s->normal_nod_mesh_patch[12*ist],
                                        wk_norm);
        };
    };
    return;
}

static void set_normal_on_node_surface_group(struct viewer_mesh *mesh_s,
                                             struct normal_nod_work *wk_norm){
    int i, ip, ip_st;
    long ist = mesh_s->ist_sf_grp_patch * mesh_s->nsurf_each_tri;
    set_surf_group_normal_4_each_node(mesh_s,
                                      &mesh_s->normal_mesh_patch[4*ist],
                                      &mesh_s->normal_nod_mesh_patch[12*ist],
                                      wk_norm->dist_nod_surf_grp);
    for (i = 0; i < mesh_s->ngrp_surf_sf; i++){
        for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
            ip_st = ip + i * mesh_s->num_pe_sf;
            refine_normal_on_node_4_grp(mesh_s, mesh_s->nnod_viewer,
                                        mesh_s->surf_stack_sf[ip_st], mesh_s->surf_stack_sf[ip_st+1],
                                        mesh_s->surf_item_sf, wk_norm->dist_nod_surf_grp,
                                        &mesh_s->normal_nod_mesh_patch[12*ist],
                                        wk_norm);
        };
    };
    return;
}

void set_normal_on_node_4_mesh(struct viewer_mesh *mesh_s){
    struct normal_nod_work *wk_norm = alloc_norm_nod_tmp(mesh_s);
    set_normal_on_domain_group(mesh_s, wk_norm);
    set_normal_on_node_element_group(mesh_s, wk_norm);
    set_normal_on_node_surface_group(mesh_s, wk_norm);
    dealloc_norm_nod_tmp(wk_norm);
	return;
}

void set_mesh_patch_group_id(struct viewer_mesh *mesh_s){
    int igrp;
    long ist;
    ist = mesh_s->ist_domain_patch * mesh_s->nsurf_each_tri;
    set_each_patch_group_id(mesh_s->num_pe_sf, mesh_s->nsurf_each_tri,
                            IZERO, mesh_s->ist_domain_grp,
                            mesh_s->isurf_stack_domain_sf, mesh_s->isurf_domain_sf,
                            &mesh_s->item_mesh_patch[mesh_s->ist_domain_patch],
                            &mesh_s->igroup_mesh_patch[ist]);
	for (igrp = 0; igrp < mesh_s->ngrp_ele_sf; igrp++){
        ist = mesh_s->ist_ele_grp_patch * mesh_s->nsurf_each_tri;
        set_each_patch_group_id(mesh_s->num_pe_sf, mesh_s->nsurf_each_tri,
                                igrp, mesh_s->ist_ele_grp,
                                mesh_s->ele_stack_sf, mesh_s->ele_item_sf,
                                &mesh_s->item_mesh_patch[mesh_s->ist_ele_grp_patch],
                                &mesh_s->igroup_mesh_patch[ist]);
    }
	for (igrp = 0; igrp < mesh_s->ngrp_surf_sf; igrp++){
        ist = mesh_s->ist_sf_grp_patch * mesh_s->nsurf_each_tri;
        set_each_patch_group_id(mesh_s->num_pe_sf, mesh_s->nsurf_each_tri,
                                igrp, mesh_s->ist_surf_grp,
                                mesh_s->surf_stack_sf, mesh_s->surf_item_sf,
                                &mesh_s->item_mesh_patch[mesh_s->ist_sf_grp_patch],
                                &mesh_s->igroup_mesh_patch[ist]);
    }
}
