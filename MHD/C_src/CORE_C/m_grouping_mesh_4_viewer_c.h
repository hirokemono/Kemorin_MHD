
/*  m_grouping_mesh_4_viewer_c.h */

#ifndef M_GROUPING_MESH_4_VIEWER_
#define M_GROUPING_MESH_4_VIEWER_

#include "calypso_param_c.h"

struct grouping_data{
	int numsurf_layer;
	int numedge_layer;
	int numnod_layer;
	
	int nnod_4_surf;
	int nnod_4_edge;
	int nedge_4_surf;
	int ntri_each_sf;
	int **node_surf_2_tri;
	
	int **ie_layer;
	int **ie_edge_layer;
	int **iedge_sf_layer;
	int *surftyp_layer;
	int *edgetyp_layer;
	
	double **rtp_layer;
	
	double **xx_l_draw;
	
	int num_comp;
	double **coef;
	char **comp_name;
};

/* Prototypes */ 

void alloc_grouping_node(struct grouping_data *mesh_g);
void alloc_grouping_sf_type(struct grouping_data *mesh_g);
void alloc_grouping_surf_param(struct grouping_data *mesh_g);
void alloc_grouping_surf_connect(struct grouping_data *mesh_g);
void alloc_grouping_edge_4_sf(struct grouping_data *mesh_g);

void alloc_grouping_mesh_draw(struct grouping_data *mesh_g);

void alloc_group_coefs(struct grouping_data *mesh_g);

void dealloc_mesh_draw_s(struct grouping_data *mesh_g);
void dealloc_group_coefs(struct grouping_data *mesh_g);
void dealloc_group_mesh(struct grouping_data *mesh_g);

#endif
