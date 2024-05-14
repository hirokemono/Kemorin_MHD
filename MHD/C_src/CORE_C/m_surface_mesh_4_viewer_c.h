
/*  m_surface_mesh_4_viewer_c.h */

#ifndef M_SURFACE_MESH_4_VIEWER_
#define M_SURFACE_MESH_4_VIEWER_

#include <stdio.h>
#include "calypso_param_c.h"

struct vect_m{
	double xyz[3];
};

struct norm_nod_t{
	struct vect_m normal[3];
	double dist_nod[3];
};

struct norm_vector{
	int num_patch;
	
	struct vect_m *norm_ele_v;
	struct norm_nod_t *norm_nod_v;
};

struct viewer_mesh {
    int num_pe_sf;
    
    int nsurf_viewer;
    int nedge_viewer;
    int nnod_viewer;
    
    int nnod_4_surf;
    int nnod_4_edge;
    int nedge_4_surf;
    int nsurf_each_tri;
    int *node_quad_2_linear_tri;
    
    int *inod_sf_stack;
    int *iedge_sf_stack;
    int *isurf_sf_stack;
    
    int *ie_sf_viewer;
    int *ie_edge_viewer;
    int *surftyp_viewer;
    int *iedge_sf_viewer;
    
    int nsurf_domain_sf;
    int nedge_domain_sf;
    int nnod_domain_sf;
    int *isurf_stack_domain_sf;
    int *edge_stack_domain_sf;
    int *nod_stack_domain_sf;
    int *isurf_domain_sf;
    int *edge_item_domain_sf;
    int *nod_item_domain_sf;
    
    int ngrp_nod_sf;
    int nnod_nod_sf;
    int *nod_stack_sf;
    int *nod_item_sf;
    
    int ngrp_ele_sf;
    int nele_ele_sf;
    int nedge_ele_sf;
    int nnod_ele_sf;
    int *ele_stack_sf;
    int *ele_edge_stack_sf;
    int *ele_nod_stack_sf;
    int *ele_item_sf;
    int *ele_edge_item_sf;
    int *ele_nod_item_sf;
    
    int ngrp_surf_sf;
    int nsurf_surf_sf;
    int nedge_surf_sf;
    int nnod_surf_sf;
    int *surf_stack_sf;
    int *surf_edge_stack_sf;
    int *surf_nod_stack_sf;
    int *surf_item_sf;
    int *surf_edge_item_sf;
    int *surf_nod_item_sf;
    
    
    double *xx_view;
    
    double *surf_size_view;
    double *surf_norm_view;
    double *surf_center_view;
    
    double *xyzw_draw;
    double *domain_center;
    double *domain_min;
    double *domain_max;
    
    double mesh_center[3];
    double xx_mesh_min[3];
    double xx_mesh_max[3];
    double rmax_mesh;
    
    
    char **subdomain_name_sf;
    
    char **nod_gp_name_sf;
    char **ele_gp_name_sf;
    char **surf_gp_name_sf;

    long ntot_groups;
    long ist_domain_grp;
    long ist_ele_grp;
    long ist_surf_grp;
    double *mesh_color;
    
    long ntot_mesh_patch;
    long ist_domain_patch;
    long ist_ele_grp_patch;
    long ist_sf_grp_patch;
    int    *item_mesh_patch;
    int    *igroup_mesh_patch;
    double *normal_mesh_patch;
    double *normal_nod_mesh_patch;
    
    long ntot_solid_patch;
    long *iele_solid_patch;
    
    long ntot_trans_patch;
    long *iele_trans_patch;
};


/* prototypes */

struct norm_vector * init_quad_norm_vector(int num_patch);
void delloc_quad_norm_vector(struct norm_vector *norms);

void alloc_nummesh_viewer_s(struct viewer_mesh *mesh_s);
void alloc_domain_stack_viewer_s(struct viewer_mesh *mesh_s);
void alloc_node_viewer_s(struct viewer_mesh *mesh_s);
void alloc_sf_type_viewer_s(struct viewer_mesh *mesh_s);
void alloc_surface_params_s(struct viewer_mesh *mesh_s);
void alloc_surf_connect_viewer_s(struct viewer_mesh *mesh_s);
void alloc_edge_4_sf_viewer_s(struct viewer_mesh *mesh_s);
void alloc_domain_nod_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_domain_surf_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_domain_edge_item_viewer_s(struct viewer_mesh *mesh_s);

void alloc_nod_grp_stack_viewer_s(struct viewer_mesh *mesh_s);
void alloc_nod_grp_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_ele_grp_stack_viewer_s(struct viewer_mesh *mesh_s);
void alloc_ele_grp_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_ele_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_ele_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_surf_grp_stack_viewer_s(struct viewer_mesh *mesh_s);
void alloc_surf_grp_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_surf_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s);
void alloc_surf_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s);

void alloc_normal_surf_viewer_s(struct viewer_mesh *mesh_s);
void alloc_domain_center_s(struct viewer_mesh *mesh_s);
void alloc_mesh_draw_s(struct viewer_mesh *mesh_s);

void alloc_mesh_normals_s(struct viewer_mesh *mesh_s);
void dealloc_mesh_normals_s(struct viewer_mesh *mesh_s);

struct viewer_mesh * alloc_viewer_mesh(void);
void dealloc_all_mesh_4_viewer_s(struct viewer_mesh *mesh_s);

#endif
