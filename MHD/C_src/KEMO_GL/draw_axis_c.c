
/* draw_axis_c.c */

#include "draw_axis_c.h"
#include <OpenGL/gl3.h>

void draw_axis_VAO(struct view_element *view_s, GLfloat dist, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *strided_buf){	
	int ncorner = ISIX;
	float radius = 3.0;
	
	GLfloat x_arrowx[6], x_arrowy[6], x_arrowz[6];
	GLfloat w_ratio[3];
	GLfloat x_charax[12], x_charay[18], x_charaz[18];
	
	int num_patch = count_axis_to_buf(ncorner);
	
	set_buffer_address_4_patch(ITHREE*num_patch, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	
	set_axis_to_buf(view_s, dist, ncorner, radius, strided_buf);
	
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glGenVertexArrays(1, &mesh_VAO->id_VAO);
	glBindVertexArray(mesh_VAO->id_VAO);
	
	glGenBuffers(1, &mesh_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(mesh_VAO);
	free(strided_buf->v_buf);
	
	return;
}

