
/* draw_patch_4_mesh_c.c */

#include "draw_patch_4_mesh_c.h"
#include <OpenGL/gl3.h>

static void set_each_mesh_tri_patch(int ie_local, int iele, int shading_mode, int polygon_mode, 
			double **xx_draw, int **ie_sf_viewer, int *node_quad_2_linear_tri, 
			double normal_ele[3], double normal_nod[9], 
			double f_color[4], int inum_buf, struct gl_strided_buffer *strided_buf){
	int inod, k, kr, k1, nd;
	
	for (k=0; k<ITHREE; k++) {
		if (iele < 0) {kr = ITHREE - k - 1;}
		else {kr = k;};
		k1 = node_quad_2_linear_tri[3*ie_local+kr] - 1;
		inod = ie_sf_viewer[abs(iele)-1][k1]-1;
		
		set_node_stride_VBO((ITHREE*inum_buf+k), strided_buf);
		
		for(nd=0;nd<3;nd++) {strided_buf->x_draw[nd] = xx_draw[inod][nd];};
		for(nd=0;nd<4;nd++) {strided_buf->c_draw[nd] = f_color[nd];};
		
		if (shading_mode == SMOOTH_SHADE) {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_nod[3*kr+nd];};
		} else {
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = normal_ele[nd];};
		};
		
		if(polygon_mode == REVERSE_POLYGON){
			for (nd = 0; nd < 3; nd++){strided_buf->n_draw[nd] = -strided_buf->n_draw[nd];};
		};
	};
	return;
};


int count_patch_VBO(int *istack_grp, int *ip_domain_far, struct viewer_mesh *mesh_s, int *iflag_domain){
	int i, ip, icou, ist, ied;
	
	int num_patch = 0;
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				num_patch = num_patch + mesh_s->nsurf_each_tri;
			};
		};
	};
	
	return num_patch;
}

void mesh_patch_VBO(struct view_element *view_s, int shading_mode, int polygon_mode, int surface_color,
			int color_mode, int color_loop, double opacity, GLfloat single_color[4], 
			int num_grp, int *istack_grp, int *item_grp, 
			double **normal_ele, double **normal_nod, int *isort_grp, 
			int *ip_domain_far, int igrp, struct viewer_mesh *mesh_s, int *iflag_domain, 
			struct VAO_ids *mesh_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *mesh_buf){
	int i, ip, icou, inum, ist, ied, j, jnum;
	int inum_buf = 0;
	double f_color[4];
	
	
	int num_patch = count_patch_VBO(istack_grp, ip_domain_far, mesh_s, iflag_domain);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	int id_lightPosition = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource.position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	GLfloat  lightposition[4] = {5.0, 5.0, -5.0,1.0};
	GLfloat white1[4] = {0.3, 0.3, 0.3, 1.0};
	GLfloat white2[4] = {0.8, 0.8, 0.8, 1.0};
	GLfloat white3[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat shine = 20.0;
	glUniform4fv(id_lightPosition, 1, lightposition);
	
	glUniform4fv(id_MaterialAmbient, 1, white2);
	glUniform4fv(id_MaterialDiffuse, 1, white1);
	glUniform4fv(id_MaterialSpecular, 1, white3);
	glUniform1f(id_MaterialShiness, shine);
	
	set_buffer_address_4_patch(3*num_patch, mesh_buf);
	resize_strided_buffer(mesh_buf->num_nod_buf, mesh_buf->ncomp_buf, mesh_buf);
	
	
	for(i = 0; i < mesh_s->num_pe_sf; i++){
		ip = ip_domain_far[i] - 1;
		if(iflag_domain[ip] != 0){
			set_patch_color_mode_c(surface_color, color_mode, color_loop, ip, mesh_s->num_pe_sf, 
						igrp, num_grp, opacity, single_color, f_color);
			
			ist = istack_grp[ip];
			ied = istack_grp[ip+1];
			for(icou = ist; icou < ied; icou++){
				inum = isort_grp[icou];
				
				for (j = 0; j < mesh_s->nsurf_each_tri; j++) {
					jnum = j + inum * mesh_s->nsurf_each_tri;
					/*
					printf("%d, %f %f %f \n", jnum, normal_ele[jnum][0],
								normal_ele[jnum][1], normal_ele[jnum][2]);
					 */
					set_each_mesh_tri_patch(j, item_grp[inum], shading_mode, polygon_mode,
								mesh_s->xx_draw, mesh_s->ie_sf_viewer, mesh_s->node_quad_2_linear_tri, 
								normal_ele[jnum], normal_nod[jnum], 
								f_color, inum_buf, mesh_buf);
					
					inum_buf = inum_buf + 1;
				};
			};
		};
	};
	
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
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*inum_buf));
	
	DestroyVBO(mesh_VAO);
	
	return;
}
