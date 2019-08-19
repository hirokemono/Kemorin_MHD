
/* draw_axis_c.c */

#include "draw_axis_c.h"
#include <OpenGL/gl3.h>

void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf){	
	int ncorner = ISIX;
	float radius = 3.0;
	
	int num_patch = count_axis_to_buf(ncorner);
	
	set_buffer_address_4_patch(ITHREE*num_patch, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	
	set_axis_to_buf(view_s, dist, ncorner, radius, strided_buf);
	
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(mesh_VAO, strided_buf);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(mesh_VAO);
	
	free(strided_buf->v_buf);
	
	return;
}

