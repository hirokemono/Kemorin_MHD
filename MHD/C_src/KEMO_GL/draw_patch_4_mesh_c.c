
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"
#include <OpenGL/gl3.h>


void draw_solid_mesh_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
/*	int i;*/
	
	copy_patch_distance_mesh(mesh_s);
	
/*
	for(i=0;i<mesh_s->nsurf_domain_sf * mesh_s->nsurf_each_tri;i++){
		printf("%d, %f %f %f \n", i, mesh_s->normal_domain[i][0],
		mesh_s->normal_domain[i][1], mesh_s->normal_domain[i][2]);
	}
*/
	
	
	int icou = 0;
	int num_patch = count_solid_mesh_patches(mesh_s, mesh_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = set_solid_mesh_patches_to_buf(mesh_s, mesh_m, mesh_buf);
	
	
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
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(mesh_VAO);
	
	return;
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

