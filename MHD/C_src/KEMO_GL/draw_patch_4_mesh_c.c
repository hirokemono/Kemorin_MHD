
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"


static int set_solid_mesh_patch_VAO(int shading_mode, 
			struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct VAO_ids *mesh_VAO, struct gl_strided_buffer *mesh_buf){
	int icou = 0;
	
	copy_patch_distance_mesh(mesh_s);
	int num_patch = count_solid_mesh_patches(mesh_s, mesh_m);
	mesh_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return num_patch;
	
	set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = set_solid_mesh_patches_to_buf(shading_mode, mesh_s, mesh_m, mesh_buf);
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	return num_patch;
}


void set_trans_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_trans_VAO){
	struct gl_strided_buffer *mesh_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	int num_patch, icou;
	
	if(mesh_m->domain_opacity < 1.0
				|| mesh_m->ele_grp_opacity < 1.0
				|| mesh_m->surf_grp_opacity < 1.0){
		sort_by_patch_distance_mesh(mesh_s, view_s);
	} else {
		copy_patch_distance_mesh(mesh_s);
	}
	
	num_patch = count_transparent_mesh_patches(mesh_s, mesh_m);
	mesh_trans_VAO->npoint_draw = ITHREE * num_patch;

	if(num_patch > 0){
		set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
		alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
		icou = 0;
		icou = set_transparent_mesh_patches_to_buf(view_s->shading_mode, mesh_s, mesh_m, mesh_buf);
		
		Const_VAO_4_Phong(mesh_trans_VAO, mesh_buf);
		free(mesh_buf->v_buf);
	};
	free(mesh_buf);
	return;
};


static int set_mesh_grids_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct VAO_ids *mesh_VAO, struct gl_strided_buffer *mesh_buf){
	int i;
	int icou;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	int num_edge = count_mesh_grid_to_buf(mesh_s, mesh_m);
	mesh_VAO->npoint_draw = ITWO * num_edge;
	if(num_edge <= 0) return num_edge;
	
	set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_mesh_grid_to_buf(mesh_s, mesh_m, mesh_buf);

	Const_VAO_4_Simple(mesh_VAO, mesh_buf);
	return num_edge;
}


static int set_mesh_nodes_ico_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct VAO_ids *mesh_VAO, struct gl_strided_buffer *mesh_buf){
	int icou;
	int num_patch = count_mesh_node_to_buf(mesh_s, mesh_m);
	mesh_VAO->npoint_draw = ITHREE * num_patch;
	if(num_patch <= 0) return 0;
	
	set_buffer_address_4_patch(3*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_mesh_node_to_buf(mesh_s, mesh_m, mesh_buf);
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
return num_patch;
}


void set_solid_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids **mesh_VAO){
	int nedge_mesh, npatch_nodes, npatch_mesh;
	
	struct gl_strided_buffer *mesh_buf
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	nedge_mesh = set_mesh_grids_VAO(mesh_s, mesh_m, mesh_VAO[1], mesh_buf);
	
	npatch_nodes = set_mesh_nodes_ico_VAO(mesh_s, mesh_m, mesh_VAO[2], mesh_buf);
	
	npatch_mesh = set_solid_mesh_patch_VAO(view_s->shading_mode, mesh_s, mesh_m, 
										   mesh_VAO[0], mesh_buf);
	
	free(mesh_buf->v_buf);
	free(mesh_buf);
	return;
};
