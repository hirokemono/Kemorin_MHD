/*
 *  set_surface_mesh_data.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/19.
 *
 */

#ifndef SET_SURFACE_MESH_DATA_
#define SET_SURFACE_MESH_DATA_

#include <math.h>
#include "m_surface_mesh_4_viewer_c.h"
#include "check_viewer_mesh_c.h"

struct normal_nod_work{
    double *dist_nod_domain;
    double *dist_nod_ele_grp;
    double *dist_nod_surf_grp;

    double *icou_nod_tmp;
    double *dist_nod_tmp;
    double *norm_nod_tmp;
};

/* prototypes */

struct normal_nod_work * alloc_norm_nod_tmp(const struct viewer_mesh *mesh_s);
void dealloc_norm_nod_tmp(struct normal_nod_work *wk_norm);

void set_surface_mesh_size(struct viewer_mesh *mesh_s);

void set_normal_4_each_node(int i, int iele, int idir, struct viewer_mesh *mesh_s,
                            double *normal, double *norm_nod, double *distance);
void refine_normal_on_node_4_grp(struct viewer_mesh *mesh_s, int ntot_nod, int ist, int ied,
                                 int *item_grp, double *dist_nod, double *norm_nod, 
                                 struct normal_nod_work *wk_norm);

void set_each_patch_group_id(int num_pe_sf, int nsurf_each_tri,
                             int igrp, long istart_group,
                             int *istack_grp, int *item_grp,
                             int *item_mesh_patch,
                             int *igroup_mesh_patch);
#endif
