
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"
#include <OpenGL/gl3.h>


int draw_solid_mesh_patch_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int icou = 0;
	
	copy_patch_distance_mesh(mesh_s);
	int num_patch = count_solid_mesh_patches(mesh_s, mesh_m);
	if(num_patch <= 0) return num_patch;
	
	set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = set_solid_mesh_patches_to_buf(mesh_s, mesh_m, mesh_buf);
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	return num_patch;
}


void draw_trans_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int num_patch, icou;
	
	if(mesh_m->domain_opacity < 1.0
				|| mesh_m->ele_grp_opacity < 1.0
				|| mesh_m->surf_grp_opacity < 1.0){
		sort_by_patch_distance_mesh(mesh_s, view_s);
	} else {
		copy_patch_distance_mesh(mesh_s);
	}
	
	num_patch = count_transparent_mesh_patches(mesh_s, mesh_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_transparent_mesh_patches_to_buf(mesh_s, mesh_m, mesh_buf);
	
	
	if (mesh_m->polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
	}
	else if(mesh_m->polygon_mode == REVERSE_POLYGON) { 
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(mesh_VAO);
	
	return;
}


int draw_mesh_grids_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int i, ip_st;
	int icou;
	
	for(i=0; i < mesh_s->num_pe_sf;i++){mesh_s->ip_domain_far[i] = i+1;};
	
	int num_edge = count_mesh_grid_to_buf(mesh_s, mesh_m);
	if(num_edge <= 0) return num_edge;
	
	set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_mesh_grid_to_buf(mesh_s, mesh_m, mesh_buf);
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	return num_edge;
}


int draw_mesh_nodes_ico_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int icou;
	int num_patch = count_mesh_node_to_buf(mesh_s, mesh_m);
	if(num_patch <= 0) return 0;
	
	set_buffer_address_4_patch(3*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_mesh_node_to_buf(mesh_s, mesh_m, mesh_buf);
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	return num_patch;
}


void draw_solid_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_solid_VAO, 
			struct VAO_ids *mesh_grid_VAO, struct VAO_ids *mesh_node_VAO, 
			struct kemoview_shaders *kemo_shaders){
	int nedge_mesh, npatch_nodes, npatch_mesh;
	
	struct gl_strided_buffer *mesh_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, mesh_buf);
	alloc_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	nedge_mesh = draw_mesh_grids_VAO(mesh_s, mesh_m, view_s, mesh_grid_VAO, kemo_shaders, mesh_buf);
	npatch_nodes = draw_mesh_nodes_ico_VAO(mesh_s, mesh_m, view_s, mesh_node_VAO, kemo_shaders, mesh_buf);
	npatch_mesh = draw_solid_mesh_patch_VAO(mesh_s, mesh_m, view_s, mesh_solid_VAO, kemo_shaders, mesh_buf);
	
	free(mesh_buf->v_buf);
	free(mesh_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(mesh_grid_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_mesh));
	
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT, GL_FILL);
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(mesh_node_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*npatch_nodes));
	
	glEnable(GL_CULL_FACE);
	if(mesh_m->polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
	} else if(mesh_m->polygon_mode == REVERSE_POLYGON) {
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glBindVertexArray(mesh_solid_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*npatch_mesh));
	/*
	Destroy_Phong_VAO(mesh_grid_VAO);
	Destroy_Phong_VAO(mesh_node_VAO);
	Destroy_Phong_VAO(mesh_solid_VAO);
	*/
};
