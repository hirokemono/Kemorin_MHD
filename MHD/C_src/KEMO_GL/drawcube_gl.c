/*
 *  drawcube_gl.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "drawcube_gl.h"

static void light_for_initial_cube(struct kemoview_shaders *kemo_shaders){
	int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
	int id_light1Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
	int id_light2Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[1].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	int num_light = 2;
	GLfloat  lightposition[4] =  { 1.5, 1.5,-10.0,0.0};
	GLfloat  light2position[4] = {-1.5,-1.5,-10.0,0.0};
	GLfloat white[4] = {0.6, 0.6, 0.6, 1.0};
	GLfloat shine = 20.0;
	
	glUniform1i(id_numLight, num_light);
	glUniform4fv(id_light1Position, 1, lightposition);
	glUniform4fv(id_light2Position, 1 , light2position);
	
	glUniform4fv(id_MaterialAmbient, 1, white);
	glUniform4fv(id_MaterialDiffuse, 1, white);
	glUniform4fv(id_MaterialSpecular, 1, white);
	glUniform1f(id_MaterialShiness, shine);
	return;
};

void set_initial_cube_VAO(struct view_element *view_s, struct VAO_ids *cube_VAO){
	struct gl_strided_buffer *cube_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, cube_buf);
	alloc_strided_buffer(cube_buf->num_nod_buf, cube_buf->ncomp_buf, cube_buf);
	
	cube_VAO->npoint_draw = 36;
	cube_surf_VBO(0.5f, cube_VAO, cube_buf);
	glBindVertexArray(0);
	
	free(cube_buf->v_buf);
	free(cube_buf);
	return;
};

void draw_initial_cube(struct view_element *view_s, struct VAO_ids *cube_VAO, 
			struct kemoview_shaders *kemo_shaders){
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	light_for_initial_cube(kemo_shaders);
	
	glBindVertexArray(cube_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
	return;
}

void draw_cube_edge_gl3(struct view_element *view_s, 
						struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *gl_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, gl_buf);
	alloc_strided_buffer(gl_buf->num_nod_buf, gl_buf->ncomp_buf, gl_buf);
	
	glGenVertexArrays(1, &cube_VAO->id_VAO);
	glBindVertexArray(cube_VAO->id_VAO);
	cube_edge_VBO(0.5f, cube_VAO, gl_buf);
	glBindVertexArray(0);
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
	
	glUseProgram(kemo_shaders->simple->programId);
	transfer_matrix_to_shader(kemo_shaders->simple, view_s);
	
	glBindVertexArray(cube_VAO->id_VAO);
	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, 0);
	
	Destroy_Simple_VAO(cube_VAO);
	glDeleteVertexArrays(1, &cube_VAO->id_VAO);
}

void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *quad_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	
	set_buffer_address_4_patch(4, quad_buf);
	alloc_strided_buffer(quad_buf->num_nod_buf, quad_buf->ncomp_buf, quad_buf);
	
	glGenVertexArrays(1, &quad_VAO->id_VAO);
	glBindVertexArray(quad_VAO->id_VAO);
	set_quadVBO(quad_VAO, quad_buf);
	glBindVertexArray(0);
	
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
	
	glUseProgram(kemo_shaders->simple->programId);
	identity_matrix_to_shader(kemo_shaders->simple);
	
	glBindVertexArray(quad_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, quad_VAO->id_index);
	glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
	
	Destroy_Simple_VAO(quad_VAO);
	glDeleteVertexArrays(1, &quad_VAO->id_VAO);
	
	free(quad_buf->v_buf);
	free(quad_buf);
	
	return;
};
