
/* draw_grids_4_mesh.c */

#include "draw_grids_4_mesh.h"
#include <OpenGL/gl3.h>

static int set_each_mesh_grid(int iedge, struct viewer_mesh *mesh_s, double f_color[4],
						  int inum_buf, struct gl_strided_buffer *strided_buf){
	int i1, i2, k1, nd;
	
	for(k1=0;k1<(mesh_s->nnod_4_edge-1);k1++){
		i1 = mesh_s->ie_edge_viewer[iedge][k1  ]-1;
		set_node_stride_VBO((ITWO*inum_buf), strided_buf);
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] =  mesh_s->xx_draw[i1][nd];};
		for(nd=0;nd<3;nd++) {strided_buf->n_draw[nd] =  0.0;};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];};
		
		i2 = mesh_s->ie_edge_viewer[iedge][k1+1]-1;
		set_node_stride_VBO((ITWO*inum_buf+1), strided_buf);
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] =  mesh_s->xx_draw[i2][nd];};
		for(nd=0;nd<3;nd++) {strided_buf->n_draw[nd] =  0.0;};
		for(nd=0;nd<4;nd++){strided_buf->c_draw[nd] = f_color[nd];};
		
		inum_buf = inum_buf + 1;
	};
	
	return inum_buf;
}

int count_mesh_edge_VBO(int *iflag_domain, int *istack_grp, struct viewer_mesh *mesh_s){
	int ip;
	int num_edge = 0;
	
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			num_edge = num_edge + (mesh_s->nnod_4_edge-1)*(istack_grp[ip+1] -istack_grp[ip]);
		};
	};
	
	return num_edge;
}

void set_mesh_edge_VBO(int line_color, int color_mode, int color_loop, GLfloat single_color[4],
			int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain, 
			struct viewer_mesh *mesh_s, struct gl_strided_buffer *mesh_buf){
	
	int ip, inum, iedge, ist, ied;
	int inum_buf;
	double f_color[4];
	
	inum_buf = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			set_grid_color_mode_c(line_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
								  igrp, num_grp, single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(inum = ist; inum < ied; inum++){
				iedge = abs( item_grp[inum] ) - 1;
				inum_buf = set_each_mesh_grid(iedge, mesh_s, f_color, inum_buf, mesh_buf);
			};
		};
	};
	
	return;
}

void mesh_edge_VBO(struct view_element *view_s, int line_color, int color_mode, int color_loop, GLfloat single_color[4],
			int num_grp, int *istack_grp, int *item_grp, int igrp, int *iflag_domain, 
			struct viewer_mesh *mesh_s, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *mesh_buf){
	
	int num_edge = count_mesh_edge_VBO(iflag_domain, istack_grp, mesh_s);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
	int id_lightPosition = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	int num_light = 1;
	GLfloat  lightposition[4] = {5.0, 5.0, -5.0,1.0};
	GLfloat white1[4] = {0.3, 0.3, 0.3, 1.0};
	GLfloat white2[4] = {0.8, 0.8, 0.8, 1.0};
	GLfloat white3[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat shine = 20.0;
	
	glUniform1i(id_numLight, num_light);
	glUniform4fv(id_lightPosition, 1, lightposition);
	
	glUniform4fv(id_MaterialAmbient, 1, white2);
	glUniform4fv(id_MaterialDiffuse, 1, white1);
	glUniform4fv(id_MaterialSpecular, 1, white3);
	glUniform1f(id_MaterialShiness, shine);
	
	set_buffer_address_4_patch(ITWO*num_edge, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	
	set_mesh_edge_VBO(line_color, color_mode, color_loop, single_color,
				num_grp, istack_grp, item_grp, igrp, iflag_domain, 
				mesh_s, mesh_buf);
	
	glGenVertexArrays(1, &mesh_VAO->id_VAO);
	glBindVertexArray(mesh_VAO->id_VAO);
	
	glGenBuffers(1, &mesh_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * mesh_buf->num_nod_buf*mesh_buf->ncomp_buf,
				 mesh_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, mesh_buf->istride,
						  (GLvoid*) (mesh_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, mesh_buf->istride, 
						  (GLvoid*) (mesh_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, mesh_buf->istride, 
						  (GLvoid*) (mesh_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(mesh_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, mesh_VAO->id_vertex);
	
	glDrawArrays(GL_LINES, IZERO, (ITWO*num_edge));
	
	DestroyVBO(mesh_VAO);
	
	return;
}
