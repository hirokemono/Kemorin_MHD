
/*  m_grouping_mesh_4_viewer_c.c */

#include <stdio.h>
#include <stdlib.h>

#include "m_grouping_mesh_4_viewer_c.h"


void alloc_grouping_node(struct grouping_data *mesh_g){
	int i;
	/* allocate memory  rtp_layer[node #][direction]*/
	mesh_g->rtp_layer = (double **)calloc(mesh_g->numnod_layer,sizeof(double *));
	for (i = 0; i < mesh_g->numnod_layer; i++){
		mesh_g->rtp_layer[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_grouping_sf_type(struct grouping_data *mesh_g){
	mesh_g->surftyp_layer =  (int *)calloc(mesh_g->numsurf_layer,sizeof(int));
	return;
};

void alloc_grouping_surf_param(struct grouping_data *mesh_g){
	int i;
	
	mesh_g->nnod_4_surf =  4;
	mesh_g->nnod_4_edge =  2;
	mesh_g->nedge_4_surf = 4;
	mesh_g->ntri_each_sf = 2;
	
	/* allocate memory  node_surf_2_tri[devided surface #][local node ID]*/
	mesh_g->node_surf_2_tri = (int **)calloc(mesh_g->ntri_each_sf,sizeof(int *));
	for (i = 0; i < mesh_g->ntri_each_sf; i++){
		mesh_g->node_surf_2_tri[i] = (int *)calloc(3,sizeof(int));
	}
	
	mesh_g->node_surf_2_tri[0][0] = 1;
	mesh_g->node_surf_2_tri[0][1] = 2;
	mesh_g->node_surf_2_tri[0][2] = 4;
	
	mesh_g->node_surf_2_tri[1][0] = 2;
	mesh_g->node_surf_2_tri[1][1] = 3;
	mesh_g->node_surf_2_tri[1][2] = 4;
	
	return;
};

void alloc_grouping_surf_connect(struct grouping_data *mesh_g){
	int i;
	/* allocate memory  ie_layer[element #][node index]*/
	mesh_g->ie_layer = (int **)calloc(mesh_g->numsurf_layer,sizeof(int *));
	for (i = 0; i < mesh_g->numsurf_layer; i++){
		mesh_g->ie_layer[i] = (int *)calloc(mesh_g->nnod_4_surf,sizeof(int));
	};
	
	return;
};

void alloc_grouping_edge_4_sf(struct grouping_data *mesh_g){
	int i;
	/* allocate memory  ie_edge_layer[edge #][local node ID]*/
	mesh_g->edgetyp_layer =  (int *)calloc(mesh_g->numedge_layer,sizeof(int));
	mesh_g->ie_edge_layer = (int **)calloc(mesh_g->numedge_layer,sizeof(int *));
	for (i = 0; i < mesh_g->numedge_layer; i++){
		mesh_g->ie_edge_layer[i] = (int *)calloc(mesh_g->nnod_4_edge,sizeof(int));
	};
	/* allocate memory  iedge_sf_layer[element #][local edge ID]*/
	mesh_g->iedge_sf_layer = (int **)calloc(mesh_g->numsurf_layer,sizeof(int *));
	for (i = 0; i < mesh_g->numsurf_layer; i++){
		mesh_g->iedge_sf_layer[i] = (int *)calloc(mesh_g->nedge_4_surf,sizeof(int));
	};
	
	return;
};


void alloc_grouping_mesh_draw(struct grouping_data *mesh_g){
	int i;
	/* allocate memory  xx_l_draw[node #][direction]*/
	mesh_g->xx_l_draw = (double **)calloc(mesh_g->numnod_layer,sizeof(double *));
	for (i = 0; i < mesh_g->numnod_layer; i++){
		mesh_g->xx_l_draw[i] = (double *)calloc(3,sizeof(double));
	};
	
	return;
};

void alloc_group_coefs(struct grouping_data *mesh_g){
	int i;
	/* allocate memory  coef[component][element #]*/
	mesh_g->comp_name = (char **)calloc(mesh_g->num_comp, sizeof(char *));
	for (i = 0; i < mesh_g->num_comp; i++) {
		mesh_g->comp_name[i] = (char *)calloc(KCHARA_C, sizeof(char));
	};
	
	mesh_g->coef = (double **)calloc(mesh_g->num_comp,sizeof(double *));
	for (i = 0; i < mesh_g->num_comp; i++){
		mesh_g->coef[i] = (double *)calloc(mesh_g->numsurf_layer,sizeof(double));
	};
	
	return;
};

static void dealloc_node_viewer_s(struct grouping_data *mesh_g){
	int i;
	for (i = 0; i < mesh_g->numnod_layer; i++) free(mesh_g->rtp_layer[i]);
	free(mesh_g->rtp_layer);
	return;
};

static void dealloc_surf_connect_viewer_s(struct grouping_data *mesh_g){
	int i;
	for (i = 0; i < mesh_g->numsurf_layer; i++) free(mesh_g->ie_layer[i]);
	free(mesh_g->ie_layer);
	for (i = 0; i < mesh_g->ntri_each_sf; i++) free(mesh_g->node_surf_2_tri[i]);
	free(mesh_g->node_surf_2_tri);
	free(mesh_g->surftyp_layer);
	return;
};

static void dealloc_edge_4_sf_viewer_s(struct grouping_data *mesh_g){
	int i;
	for (i = 0; i < mesh_g->numedge_layer; i++) free(mesh_g->ie_edge_layer[i]);
	free(mesh_g->ie_edge_layer);
	for (i = 0; i < mesh_g->numsurf_layer; i++) free(mesh_g->iedge_sf_layer[i]);
	free(mesh_g->iedge_sf_layer);
	free(mesh_g->edgetyp_layer);
	return;
};

void dealloc_mesh_draw_s(struct grouping_data *mesh_g){
	int i;
	/* deallocate memory  xx_l_draw[node #][direction]*/
	for (i = 0; i < mesh_g->numnod_layer; i++) free(mesh_g->xx_l_draw[i]);
	free(mesh_g->xx_l_draw);
	
	return;
};


void dealloc_group_coefs(struct grouping_data *mesh_g){
	int i;
	
	for (i = 0; i < mesh_g->num_comp; i++) free(mesh_g->coef[i]);
	free(mesh_g->coef);
	for (i = 0; i < KCHARA_C; i++) free(mesh_g->comp_name[i]);
	free(mesh_g->comp_name);
	return;
};

void dealloc_group_mesh(struct grouping_data *mesh_g){
	dealloc_node_viewer_s(mesh_g);
	dealloc_surf_connect_viewer_s(mesh_g);
	dealloc_edge_4_sf_viewer_s(mesh_g);
};
