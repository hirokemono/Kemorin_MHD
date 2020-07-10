
/*  m_surface_mesh_4_viewer_c.h */

#include <stdio.h>
#include <stdlib.h>

#include "m_surface_mesh_4_viewer_c.h"

struct norm_vector * init_quad_norm_vector(int num_patch){
	struct norm_vector *norms = (struct norm_vector *) malloc(sizeof(struct norm_vector));
    if (norms == NULL) {
        printf("malloc error\n");
        exit(0);
    }
	norms->num_patch = num_patch;
	
	norms->norm_ele_v = (struct vect_m *) malloc(norms->num_patch * sizeof(struct vect_m));
	norms->norm_nod_v = (struct norm_nod_t *) malloc(norms->num_patch * sizeof(struct norm_nod_t));
	
	return norms;
};

void delloc_quad_norm_vector(struct norm_vector *norms){
	free(norms->norm_ele_v);
	free(norms->norm_nod_v);
	free(norms);
	return;
};

void alloc_nummesh_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	
	mesh_s->subdomain_name_sf =  (char **)calloc((mesh_s->num_pe_sf),sizeof(char *));
	for (i = 0; i < mesh_s->num_pe_sf; i++) {
		mesh_s->subdomain_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	mesh_s->inod_sf_stack =  (int *)calloc((mesh_s->num_pe_sf)+1,sizeof(int));
	mesh_s->isurf_sf_stack = (int *)calloc((mesh_s->num_pe_sf)+1,sizeof(int));
	mesh_s->iedge_sf_stack = (int *)calloc((mesh_s->num_pe_sf)+1,sizeof(int));
	
	for (i = 0; i < mesh_s->num_pe_sf; i++) {
		sprintf(mesh_s->subdomain_name_sf[i], "Domain %d", i);
	}
	return;
};

void alloc_node_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	/* allocate memory  xx_view[node #][direction]*/
	mesh_s->xx_view = (double **)calloc(mesh_s->nnod_viewer,sizeof(double *));
	for (i = 0; i < mesh_s->nnod_viewer; i++){
		mesh_s->xx_view[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_sf_type_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->surftyp_viewer =  (int *)calloc(mesh_s->nsurf_viewer,sizeof(int));
	return;
};


void alloc_surface_params_s(struct viewer_mesh *mesh_s){
	
	if( mesh_s->surftyp_viewer[0] == 223 ){
		mesh_s->nnod_4_surf =    9;
		mesh_s->nnod_4_edge =    3;
		mesh_s->nedge_4_surf =   4;
		mesh_s->nsurf_each_tri = 8;
	}
	else if( mesh_s->surftyp_viewer[0] == 222 ){
		mesh_s->nnod_4_surf =    8;
		mesh_s->nnod_4_edge =    3;
		mesh_s->nedge_4_surf =   4;
		mesh_s->nsurf_each_tri = 6;
	}
	else{
		mesh_s->nnod_4_surf =    4;
		mesh_s->nnod_4_edge =    2;
		mesh_s->nedge_4_surf =   4;
		mesh_s->nsurf_each_tri = 2;
	}
	
	/* allocate memory  node_quad_2_linear_tri[3*(divided surface #) + local node ID]*/
	mesh_s->node_quad_2_linear_tri = (int *)calloc((3*mesh_s->nsurf_each_tri),sizeof(int));
	
	if( mesh_s->surftyp_viewer[0] == 223 ){
		mesh_s->node_quad_2_linear_tri[0] =  1;
		mesh_s->node_quad_2_linear_tri[1] =  5;
		mesh_s->node_quad_2_linear_tri[2] =  9;

		mesh_s->node_quad_2_linear_tri[3] =  9;
		mesh_s->node_quad_2_linear_tri[4] =  8;
		mesh_s->node_quad_2_linear_tri[5] =  1;

        mesh_s->node_quad_2_linear_tri[6] =  5;
		mesh_s->node_quad_2_linear_tri[7] =  2;
		mesh_s->node_quad_2_linear_tri[8] =  6;

        mesh_s->node_quad_2_linear_tri[9] =  6;
		mesh_s->node_quad_2_linear_tri[10] = 9;
		mesh_s->node_quad_2_linear_tri[11] = 5;
		
		mesh_s->node_quad_2_linear_tri[12] = 9;
		mesh_s->node_quad_2_linear_tri[13] = 6;
		mesh_s->node_quad_2_linear_tri[14] = 3;

        mesh_s->node_quad_2_linear_tri[15] = 3;
        mesh_s->node_quad_2_linear_tri[16] = 7;
        mesh_s->node_quad_2_linear_tri[17] = 9;

        mesh_s->node_quad_2_linear_tri[18] = 8;
        mesh_s->node_quad_2_linear_tri[19] = 9;
        mesh_s->node_quad_2_linear_tri[20] = 7;

        mesh_s->node_quad_2_linear_tri[21] = 7;
        mesh_s->node_quad_2_linear_tri[22] = 4;
        mesh_s->node_quad_2_linear_tri[23] = 8;
}
	else if( mesh_s->surftyp_viewer[0] == 222 ){
		mesh_s->node_quad_2_linear_tri[0] =  5;
		mesh_s->node_quad_2_linear_tri[1] =  6;
		mesh_s->node_quad_2_linear_tri[2] =  7;
		
		mesh_s->node_quad_2_linear_tri[3] =  7;
		mesh_s->node_quad_2_linear_tri[4] =  8;
		mesh_s->node_quad_2_linear_tri[5] =  5;

		mesh_s->node_quad_2_linear_tri[6] =  1;
		mesh_s->node_quad_2_linear_tri[7] =  5;
		mesh_s->node_quad_2_linear_tri[8] =  8;
		
		mesh_s->node_quad_2_linear_tri[9] =  2;
		mesh_s->node_quad_2_linear_tri[10] = 6;
		mesh_s->node_quad_2_linear_tri[11] = 5;
		
		mesh_s->node_quad_2_linear_tri[12] = 3;
		mesh_s->node_quad_2_linear_tri[13] = 7;
		mesh_s->node_quad_2_linear_tri[14] = 6;
		
		mesh_s->node_quad_2_linear_tri[15] = 4;
		mesh_s->node_quad_2_linear_tri[16] = 8;
		mesh_s->node_quad_2_linear_tri[17] = 7;
	}
	else{
		mesh_s->node_quad_2_linear_tri[0] = 1;
		mesh_s->node_quad_2_linear_tri[1] = 2;
		mesh_s->node_quad_2_linear_tri[2] = 3;
        
		mesh_s->node_quad_2_linear_tri[3] = 3;
        mesh_s->node_quad_2_linear_tri[4] = 4;
        mesh_s->node_quad_2_linear_tri[5] = 1;
	}
	
	return;
};


void alloc_surf_connect_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	/* allocate memory  ie_sf_viewer[element #][node index]*/
	mesh_s->ie_sf_viewer = (int **)calloc(mesh_s->nsurf_viewer,sizeof(int *));
	for (i = 0; i < mesh_s->nsurf_viewer; i++){
		mesh_s->ie_sf_viewer[i] = (int *)calloc(mesh_s->nnod_4_surf,sizeof(int));
	};
	return;
};

void alloc_edge_4_sf_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	/* allocate memory  ie_edge_viewer[edge #][local node ID]*/
	mesh_s->ie_edge_viewer = (int **)calloc(mesh_s->nedge_viewer,sizeof(int *));
	for (i = 0; i < mesh_s->nedge_viewer; i++){
		mesh_s->ie_edge_viewer[i] = (int *)calloc(mesh_s->nnod_4_edge,sizeof(int));
	};
	/* allocate memory  iedge_sf_viewer[element #][local edge ID]*/
	mesh_s->iedge_sf_viewer = (int **)calloc(mesh_s->nsurf_viewer,sizeof(int *));
	for (i = 0; i < mesh_s->nsurf_viewer; i++){
		mesh_s->iedge_sf_viewer[i] = (int *)calloc(mesh_s->nedge_4_surf,sizeof(int));
	};
	
	return;
};

void alloc_normal_surf_viewer_s(struct viewer_mesh *mesh_s){
	int i, num;
	num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
	/* allocate memory  surf_norm_view[devided surface #][component] */
	/* allocate memory  surf_center_view[devided surface #][component] */
	mesh_s->surf_norm_view = (double **)calloc(num,sizeof(double *));
	for (i = 0; i < num; i++){
		mesh_s->surf_norm_view[i] = (double *)calloc(3,sizeof(double));
	};
	mesh_s->surf_center_view = (double **)calloc(num,sizeof(double *));
	for (i = 0; i < num; i++){
		mesh_s->surf_center_view[i] = (double *)calloc(3,sizeof(double));
	};
	mesh_s->surf_size_view = (double *)calloc(num,sizeof(double));
	
	mesh_s->z_ele_view = (double *)calloc(num,sizeof(double));
	
	return;
};

void alloc_domain_stack_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->nod_stack_domain_sf = (int *)calloc(mesh_s->num_pe_sf+1,sizeof(int));
	mesh_s->isurf_stack_domain_sf = (int *)calloc(mesh_s->num_pe_sf+1,sizeof(int));
	mesh_s->edge_stack_domain_sf = (int *)calloc(mesh_s->num_pe_sf+1,sizeof(int));
	
	return;
};

void alloc_domain_surf_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->isurf_domain_sf = (int *)calloc(mesh_s->nsurf_domain_sf,sizeof(int));
	mesh_s->iele_domain_far = (int *)calloc(mesh_s->nsurf_domain_sf,sizeof(int));
	mesh_s->z_domain_view = (double *)calloc(mesh_s->nsurf_domain_sf,sizeof(double));
	return;
};
void alloc_domain_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->nod_item_domain_sf = (int *)calloc(mesh_s->nnod_domain_sf,sizeof(int));
	return;
};
void alloc_domain_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->edge_item_domain_sf = (int *)calloc(mesh_s->nedge_domain_sf,sizeof(int));
	return;
};


void alloc_nod_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i, num;
	num = mesh_s->num_pe_sf*mesh_s->ngrp_nod_sf+1;
	mesh_s->nod_stack_sf = (int *)calloc(num,sizeof(int));
	
	mesh_s->nod_gp_name_sf = (char **)calloc(mesh_s->ngrp_nod_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++) {
		mesh_s->nod_gp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
};

void alloc_ele_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i, num;
	num = mesh_s->num_pe_sf*mesh_s->ngrp_ele_sf+1;
	mesh_s->ele_stack_sf =      (int *)calloc(num,sizeof(int));
	mesh_s->ele_edge_stack_sf = (int *)calloc(num,sizeof(int));
	mesh_s->ele_nod_stack_sf =  (int *)calloc(num,sizeof(int));
	
	mesh_s->ele_gp_name_sf = (char **)calloc(mesh_s->ngrp_ele_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) {
		mesh_s->ele_gp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
};

void alloc_surf_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i, num;
	num = mesh_s->num_pe_sf*mesh_s->ngrp_surf_sf+1;
	mesh_s->surf_stack_sf =      (int *)calloc(num,sizeof(int));
	mesh_s->surf_edge_stack_sf = (int *)calloc(num,sizeof(int));
	mesh_s->surf_nod_stack_sf =  (int *)calloc(num,sizeof(int));
	
	mesh_s->surf_gp_name_sf = (char **)calloc(mesh_s->ngrp_surf_sf, sizeof(char *));
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) {
		mesh_s->surf_gp_name_sf[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	return;
};

void alloc_nod_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->nod_item_sf = (int *)calloc(mesh_s->nnod_nod_sf,sizeof(int));
	return;
};


void alloc_ele_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->ele_item_sf = (int *)calloc(mesh_s->nele_ele_sf,sizeof(int));
	mesh_s->iele_grp_far = (int *)calloc(mesh_s->nele_ele_sf,sizeof(int));
	mesh_s->z_ele_grp_view = (double *)calloc(mesh_s->nele_ele_sf,sizeof(double));
	return;
};
void alloc_ele_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->ele_nod_item_sf = (int *)calloc(mesh_s->nnod_ele_sf,sizeof(int));
	return;
};
void alloc_ele_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->ele_edge_item_sf = (int *)calloc(mesh_s->nedge_ele_sf,sizeof(int));
	return;
};


void alloc_surf_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->surf_item_sf = (int *)calloc(mesh_s->nsurf_surf_sf,sizeof(int));
	mesh_s->isurf_grp_far = (int *)calloc(mesh_s->nsurf_surf_sf,sizeof(int));
	mesh_s->z_surf_grp_view = (double *)calloc(mesh_s->nsurf_surf_sf,sizeof(double));
	return;
};
void alloc_surf_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->surf_nod_item_sf = (int *)calloc(mesh_s->nnod_surf_sf,sizeof(int));
	return;
};
void alloc_surf_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	mesh_s->surf_edge_item_sf = (int *)calloc(mesh_s->nedge_surf_sf,sizeof(int));
	return;
};


void alloc_domain_center_s(struct viewer_mesh *mesh_s){
	int i;
	
	/* allocate memory  domain_center[domain #][direction]*/
	/* allocate memory  domain_min[domain #][direction]*/
	/* allocate memory  domain_max[domain #][direction]*/
	mesh_s->domain_center = (double **)calloc(mesh_s->num_pe_sf,sizeof(double *));
	for (i = 0; i < mesh_s->num_pe_sf; i++){
		mesh_s->domain_center[i] = (double *)calloc(3,sizeof(double));
	};
	mesh_s->domain_min = (double **)calloc(mesh_s->num_pe_sf,sizeof(double *));
	for (i = 0; i < mesh_s->num_pe_sf; i++){
		mesh_s->domain_min[i] = (double *)calloc(3,sizeof(double));
	};
	mesh_s->domain_max = (double **)calloc(mesh_s->num_pe_sf,sizeof(double *));
	for (i = 0; i < mesh_s->num_pe_sf; i++){
		mesh_s->domain_max[i] = (double *)calloc(3,sizeof(double));
	};

	mesh_s->z_center_view = (double *)calloc(mesh_s->num_pe_sf,sizeof(double));
	mesh_s->ip_domain_far = (int *)calloc(mesh_s->num_pe_sf,sizeof(int));
	
	return;
};

void alloc_mesh_draw_s(struct viewer_mesh *mesh_s){
	int i, num;
	/* allocate memory  xx_draw[node #][direction]*/
	mesh_s->xx_draw = (double **)calloc(mesh_s->nnod_viewer,sizeof(double *));
	for (i = 0; i < mesh_s->nnod_viewer; i++){
		mesh_s->xx_draw[i] = (double *)calloc(3,sizeof(double));
	};
	


	num = mesh_s->nsurf_domain_sf * mesh_s->nsurf_each_tri;
	mesh_s->normal_domain =   (double **)calloc(num,sizeof(double *));
	mesh_s->norm_nod_domain = (double **)calloc(num,sizeof(double *));
	mesh_s->dist_nod_domain = (double **)calloc(num,sizeof(double *));
	for (i = 0; i < num; i++){
		mesh_s->normal_domain[i] =   (double *)calloc( 3,sizeof(double));
		mesh_s->norm_nod_domain[i] = (double *)calloc( 9,sizeof(double));
		mesh_s->dist_nod_domain[i] = (double *)calloc( 3,sizeof(double));
	};
	
	num = mesh_s->nele_ele_sf * mesh_s->nsurf_each_tri;
	mesh_s->normal_ele_grp = (double **)calloc(num,sizeof(double *));
	mesh_s->norm_nod_ele_grp = (double **)calloc(num,sizeof(double *));
	mesh_s->dist_nod_ele_grp = (double **)calloc(num,sizeof(double *));
	for (i = 0; i < num; i++){
		mesh_s->normal_ele_grp[i] =   (double *)calloc( 3,sizeof(double));
		mesh_s->norm_nod_ele_grp[i] = (double *)calloc( 9,sizeof(double));
		mesh_s->dist_nod_ele_grp[i] = (double *)calloc( 3,sizeof(double));
	};
	
	num = mesh_s->nsurf_surf_sf * mesh_s->nsurf_each_tri;
	mesh_s->normal_surf_grp =   (double **)calloc(num,sizeof(double *));
	mesh_s->norm_nod_surf_grp = (double **)calloc(num,sizeof(double *));
	mesh_s->dist_nod_surf_grp = (double **)calloc(num,sizeof(double *));
	for (i = 0; i < num; i++){
		mesh_s->normal_surf_grp[i] =   (double *)calloc( 3,sizeof(double));
		mesh_s->norm_nod_surf_grp[i] = (double *)calloc( 9,sizeof(double));
		mesh_s->dist_nod_surf_grp[i] = (double *)calloc( 3,sizeof(double));
	};
	
	return;
};




static void dealloc_nummesh_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	
	free(mesh_s->inod_sf_stack);
	free(mesh_s->isurf_sf_stack);
	free(mesh_s->iedge_sf_stack);
	
	for (i = 0; i < mesh_s->num_pe_sf; i++) free(mesh_s->subdomain_name_sf[i]);
	free(mesh_s->subdomain_name_sf);
	return;
};

static void dealloc_node_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	for (i = 0; i < mesh_s->nnod_viewer; i++) free(mesh_s->xx_view[i]);
	free(mesh_s->xx_view);
	return;
};

static void dealloc_surf_connect_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	
	for (i = 0; i < mesh_s->nsurf_viewer; i++) free(mesh_s->ie_sf_viewer[i]);
	free(mesh_s->ie_sf_viewer);
	
	free(mesh_s->node_quad_2_linear_tri);
	free(mesh_s->surftyp_viewer);
	return;
};


static void dealloc_edge_4_sf_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	for (i = 0; i < mesh_s->nedge_viewer; i++) free(mesh_s->ie_edge_viewer[i]);
	free(mesh_s->ie_edge_viewer);
	for (i = 0; i < mesh_s->nsurf_viewer; i++) free(mesh_s->iedge_sf_viewer[i]);
	free(mesh_s->iedge_sf_viewer);
	
	return;
};

static void dealloc_normal_surf_viewer_s(struct viewer_mesh *mesh_s){
	int i, num;
	num = mesh_s->nsurf_each_tri * mesh_s->nsurf_viewer;
	for (i = 0; i < num; i++) free(mesh_s->surf_norm_view[i]);
	free(mesh_s->surf_norm_view);
	for (i = 0; i < num; i++) free(mesh_s->surf_center_view[i]);
	free(mesh_s->surf_center_view);
	free(mesh_s->surf_size_view);
	free(mesh_s->z_ele_view);
	
	return;
};

static void dealloc_domain_stack_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->nod_stack_domain_sf);
	free(mesh_s->isurf_stack_domain_sf);
	free(mesh_s->edge_stack_domain_sf);
	
	return;
};

static void dealloc_domain_surf_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->z_domain_view);
	free(mesh_s->iele_domain_far);
	free(mesh_s->isurf_domain_sf);
	return;
};
static void dealloc_domain_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->nod_item_domain_sf);
	return;
};
static void dealloc_domain_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->edge_item_domain_sf);
	return;
};


static void dealloc_nod_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	free(mesh_s->nod_stack_sf);
	
	for (i = 0; i < mesh_s->ngrp_nod_sf; i++) free(mesh_s->nod_gp_name_sf[i]);
	free(mesh_s->nod_gp_name_sf);
	return;
};

static void dealloc_ele_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	free(mesh_s->ele_stack_sf);
	free(mesh_s->ele_edge_stack_sf);
	free(mesh_s->ele_nod_stack_sf);
	
	for (i = 0; i < mesh_s->ngrp_ele_sf; i++) free(mesh_s->ele_gp_name_sf[i]);
	free(mesh_s->ele_gp_name_sf);
	return;
};

static void dealloc_surf_grp_stack_viewer_s(struct viewer_mesh *mesh_s){
	int i;
	free(mesh_s->surf_stack_sf);
	free(mesh_s->surf_edge_stack_sf);
	free(mesh_s->surf_nod_stack_sf);
	
	for (i = 0; i < mesh_s->ngrp_surf_sf; i++) free(mesh_s->surf_gp_name_sf[i]);
	free(mesh_s->surf_gp_name_sf);
	return;
};

static void dealloc_nod_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->nod_item_sf);
	return;
};

static void dealloc_ele_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->z_ele_grp_view);
	free(mesh_s->iele_grp_far);
	free(mesh_s->ele_item_sf);
	return;
};
static void dealloc_ele_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->ele_nod_item_sf);
	return;
};
static void dealloc_ele_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->ele_edge_item_sf);
	return;
};

static void dealloc_surf_grp_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->z_surf_grp_view);
	free(mesh_s->isurf_grp_far);
	free(mesh_s->surf_item_sf);
	return;
};
static void dealloc_surf_grp_nod_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->surf_nod_item_sf);
	return;
};
static void dealloc_surf_grp_edge_item_viewer_s(struct viewer_mesh *mesh_s){
	free(mesh_s->surf_edge_item_sf);
	return;
};


static void dealloc_domain_center_s(struct viewer_mesh *mesh_s){
	int i;
	
	free(mesh_s->ip_domain_far);
	free(mesh_s->z_center_view);

	for (i = 0; i < mesh_s->num_pe_sf; i++) free(mesh_s->domain_center[i]);
	free(mesh_s->domain_center);
	for (i = 0; i < mesh_s->num_pe_sf; i++) free(mesh_s->domain_min[i]);
	free(mesh_s->domain_min);
	for (i = 0; i < mesh_s->num_pe_sf; i++) free(mesh_s->domain_max[i]);
	free(mesh_s->domain_max);
	
	return;
};

static void dealloc_mesh_draw_s(struct viewer_mesh *mesh_s){
	int i, num;

	num = mesh_s->nsurf_surf_sf * mesh_s->nsurf_each_tri;
	for (i = 0; i < num; i++) free(mesh_s->normal_surf_grp[i]);
	for (i = 0; i < num; i++) free(mesh_s->norm_nod_surf_grp[i]);
	for (i = 0; i < num; i++) free(mesh_s->dist_nod_surf_grp[i]);
	free(mesh_s->normal_surf_grp);
	free(mesh_s->norm_nod_surf_grp);
	free(mesh_s->dist_nod_surf_grp);

	num = mesh_s->nele_ele_sf * mesh_s->nsurf_each_tri;
	for (i = 0; i < num; i++) free(mesh_s->normal_ele_grp[i]);
	for (i = 0; i < num; i++) free(mesh_s->norm_nod_ele_grp[i]);
	for (i = 0; i < num; i++) free(mesh_s->dist_nod_ele_grp[i]);
	free(mesh_s->normal_ele_grp);
	free(mesh_s->norm_nod_ele_grp);
	free(mesh_s->dist_nod_ele_grp);

	num = mesh_s->nsurf_domain_sf * mesh_s->nsurf_each_tri;
	for (i = 0; i < num; i++) free(mesh_s->normal_domain[i]);
	for (i = 0; i < num; i++) free(mesh_s->norm_nod_domain[i]);
	for (i = 0; i < num; i++) free(mesh_s->dist_nod_domain[i]);
	free(mesh_s->normal_domain);
	free(mesh_s->norm_nod_domain);
	free(mesh_s->dist_nod_domain);
	
	/* deallocate memory  xx_draw[node #][direction]*/
	for (i = 0; i < mesh_s->nnod_viewer; i++) free(mesh_s->xx_draw[i]);
	free(mesh_s->xx_draw);
	
	return;
};

struct viewer_mesh * alloc_viewer_mesh(void){
    struct viewer_mesh * mesh_s =  (struct viewer_mesh *)   malloc(sizeof(struct viewer_mesh));
    if(mesh_s == NULL){
        printf("malloc error for viewer_mesh\n");
        exit(0);
    }

    return mesh_s;
};

void dealloc_all_mesh_4_viewer_s(struct viewer_mesh *mesh_s){
	dealloc_mesh_draw_s(mesh_s);
	dealloc_domain_center_s(mesh_s);
	dealloc_surf_grp_edge_item_viewer_s(mesh_s);
	dealloc_surf_grp_nod_item_viewer_s(mesh_s);
	dealloc_surf_grp_item_viewer_s(mesh_s);
	dealloc_ele_grp_edge_item_viewer_s(mesh_s);
	dealloc_ele_grp_nod_item_viewer_s(mesh_s);
	dealloc_ele_grp_item_viewer_s(mesh_s);
	dealloc_nod_grp_item_viewer_s(mesh_s);
	dealloc_surf_grp_stack_viewer_s(mesh_s);
	dealloc_ele_grp_stack_viewer_s(mesh_s);
	dealloc_nod_grp_stack_viewer_s(mesh_s);
	dealloc_domain_edge_item_viewer_s(mesh_s);
	dealloc_domain_nod_item_viewer_s(mesh_s);
	dealloc_domain_surf_item_viewer_s(mesh_s);
	dealloc_domain_stack_viewer_s(mesh_s);
	dealloc_normal_surf_viewer_s(mesh_s);
	dealloc_edge_4_sf_viewer_s(mesh_s);
	dealloc_surf_connect_viewer_s(mesh_s);
	dealloc_node_viewer_s(mesh_s);
	dealloc_nummesh_viewer_s(mesh_s);
	
	return;
}
