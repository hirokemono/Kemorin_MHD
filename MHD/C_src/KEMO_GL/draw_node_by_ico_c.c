
/* draw_node_by_ico_c.c */

#include "draw_node_by_ico_c.h"
#include <OpenGL/gl3.h>


static int count_node_ico_VBO(int *istack_grp, struct viewer_mesh *mesh_s, int *iflag_domain){
	int ip;
	int num_patch = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			num_patch = num_patch + 20 * (istack_grp[ip+1] - istack_grp[ip]);
		}
	};
	
	return num_patch;
}

static void set_node_ico_VBO(int num_grp, int igrp, int *istack_grp, int *item_grp,
			struct viewer_mesh *mesh_s, double node_diam,
			int node_color, int color_mode, int color_loop, GLfloat single_color[4], 
			int *iflag_domain, struct gl_strided_buffer *mesh_buf){
	double f_color[4];
	double xyz_patch[180], norm_patch[180];
	int inum_buf, num_ico;
	int i, nd, ip, inod, inum, ist, ied;
	
	inum_buf = 0;
	for(ip = 0; ip < mesh_s->num_pe_sf; ip++){
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			set_patch_color_mode_c(node_color, color_mode, color_loop, (ip+1),
				mesh_s->num_pe_sf, igrp, num_grp, ONE, single_color, f_color);
			
			for(inum = ist; inum < ied; inum++){
				inod = item_grp[inum]-1;
				num_ico = set_icosahedron_patch(node_diam, &mesh_s->xx_draw[inod][0],
												 xyz_patch, norm_patch);
				for (i=0; i<num_ico; i++) {
					set_node_stride_VBO((inum_buf+i), mesh_buf);
					for(nd=0;nd<3;nd++){mesh_buf->x_draw[nd] =  xyz_patch[3*i+nd];};
					for(nd=0;nd<3;nd++){mesh_buf->n_draw[nd] = norm_patch[3*i+nd];};
					for(nd=0;nd<4;nd++){mesh_buf->c_draw[nd] = f_color[nd];};
				};
				
				inum_buf = inum_buf + num_ico;
			};
		}
	};
	
	return;
}

void node_ico_VBO(struct view_element *view_s, 
			int num_grp, int igrp, int *istack_grp, int *item_grp,
			struct viewer_mesh *mesh_s, double node_diam,
			int node_color, int color_mode, int color_loop, GLfloat single_color[4], int *iflag_domain, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *mesh_buf){
	int num_patch = count_node_ico_VBO(istack_grp, mesh_s, iflag_domain);
	
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
	
	set_buffer_address_4_patch(3*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	
	set_node_color_mode_c(node_color, color_mode, color_loop, igrp, num_grp, single_color);
	set_node_ico_VBO(num_grp, igrp, istack_grp, item_grp, mesh_s, node_diam,
			node_color, color_mode, color_loop, single_color, iflag_domain, mesh_buf);
	
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
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	DestroyVBO(mesh_VAO);
	return;
}
