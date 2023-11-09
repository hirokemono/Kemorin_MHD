/*
 *  drawcube_gl.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "drawcube_gl.h"

static const int    num_light = 2;
static const float  lightposition[4] =  { 1.5, 1.5,-10.0,0.0};
static const float  light2position[4] = {-1.5,-1.5,-10.0,0.0};
static const float  white1[4] = {0.4, 0.4, 0.4, 1.0};
static const float  white2[4] = {0.4, 0.4, 0.4, 1.0};
static const float  white3[4] = {0.2, 0.2, 0.2, 1.0};
static const float  shine[1] = {30.0};


struct initial_cube_lighting * init_inital_cube_lighting(void){
    struct initial_cube_lighting *init_light
        = (struct initial_cube_lighting *) malloc(sizeof(struct initial_cube_lighting));
    if(init_light == NULL){
        printf("malloc error for initial_cube_lighting\n");
        exit(0);
    }
    init_light->num_light = num_light;
    for(int i=0;i<4;i++){
        init_light->lightposition[0][i] = lightposition[i];
        init_light->lightposition[1][i] = light2position[i];
        init_light->whitelight[0][i] = white1[i];
        init_light->whitelight[1][i] = white2[i];
        init_light->whitelight[2][i] = white3[i];
    };
    init_light->shine[0] = shine[0];

    return init_light;
};

static void light_for_initial_cube(struct initial_cube_lighting *init_light,
                                   struct kemoview_shaders *kemo_shaders){
    int id_numLight = glGetUniformLocation(kemo_shaders->phong->programId, "num_lights");
	int id_light1Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[0].position");
	int id_light2Position = glGetUniformLocation(kemo_shaders->phong->programId, "LightSource[1].position");
	
	int id_MaterialAmbient = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.ambient");
	int id_MaterialDiffuse = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.diffuse");
	int id_MaterialSpecular = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.specular");
	int id_MaterialShiness = glGetUniformLocation(kemo_shaders->phong->programId, "frontMaterial.shininess");
	
	glUniform1i(id_numLight, init_light->num_light);
	glUniform4fv(id_light1Position, 1, init_light->lightposition[0]);
	glUniform4fv(id_light2Position, 1 , init_light->lightposition[1]);
	
	glUniform4fv(id_MaterialAmbient, 1, init_light->whitelight[0]);
	glUniform4fv(id_MaterialDiffuse, 1, init_light->whitelight[1]);
	glUniform4fv(id_MaterialSpecular, 1, init_light->whitelight[2]);
	glUniform1fv(id_MaterialShiness, 1, init_light->shine);
	return;
};

void const_initial_cube_buffer(struct gl_strided_buffer *cube_buf){
    set_buffer_address_4_patch(8, cube_buf);
    alloc_strided_buffer(cube_buf);
    CubeNode_to_buf(0.5f, cube_buf);
    return;
};

void set_initial_cube_VAO(struct gl_strided_buffer *cube_buf, struct VAO_ids *cube_VAO){
	cube_VAO->npoint_draw = 36;
	cube_surf_VBO(cube_VAO, cube_buf);
	glBindVertexArray(0);
	
	free(cube_buf->v_buf);
	return;
};

void draw_initial_cube(struct transfer_matrices *matrices, struct initial_cube_lighting *init_light,
                       struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
    glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
	light_for_initial_cube(init_light, kemo_shaders);

	glBindVertexArray(cube_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
	return;
}

void draw_cube_edge_gl3(struct view_element *view_s, 
						struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
    struct transfer_matrices *matrices;
    
	struct gl_strided_buffer *gl_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, gl_buf);
	alloc_strided_buffer(gl_buf);
    CubeNode_to_buf(0.5f, gl_buf);

	glGenVertexArrays(1, &cube_VAO->id_VAO);
	glBindVertexArray(cube_VAO->id_VAO);
	cube_edge_VBO(cube_VAO, gl_buf);
	glBindVertexArray(0);
	
    set_gl_animation_rot_angle(view_s, 0);
	update_projection_struct(view_s);
	modify_view_by_struct(view_s);
    matrices = transfer_matrix_to_shader(view_s);

	glUseProgram(kemo_shaders->simple->programId);
    transfer_matrix_to_GL(kemo_shaders->simple, matrices);
    free(matrices);

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
	alloc_strided_buffer(quad_buf);
	
	glGenVertexArrays(1, &quad_VAO->id_VAO);
	glBindVertexArray(quad_VAO->id_VAO);
	set_quadVBO(quad_VAO, quad_buf);
	glBindVertexArray(0);
	
    set_gl_animation_rot_angle(view_s, 0);
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
