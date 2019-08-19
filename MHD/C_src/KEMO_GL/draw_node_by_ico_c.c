
/* draw_node_by_ico_c.c */

#include "draw_node_by_ico_c.h"
#include <OpenGL/gl3.h>


void draw_mesh_nodes_ico_VAO(struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
			struct view_element *view_s, struct VAO_ids *mesh_VAO, 
			struct kemoview_shaders *kemo_shaders, struct gl_strided_buffer *mesh_buf){
	int icou;
	int num_patch = count_mesh_node_to_buf(mesh_s, mesh_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(3*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	icou = 0;
	icou = set_mesh_node_to_buf(mesh_s, mesh_m, mesh_buf);
	
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT, GL_FILL);
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(mesh_VAO, mesh_buf);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(mesh_VAO);
	
	return;
}
