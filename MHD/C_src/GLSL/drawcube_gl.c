/*
 *  drawcube_gl.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "drawcube_gl.h"


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

void set_initial_cube_VAO(struct initial_cube_buffers *initial_bufs,
                          struct VAO_ids *cube_VAO){
    cube_VAO->npoint_draw = initial_bufs->cube_index_buf->ntot_vertex;
	if(cube_VAO->npoint_draw <= 0) return;
	cube_surf_VBO(cube_VAO, initial_bufs->cube_buf, initial_bufs->cube_index_buf);
	glBindVertexArray(0);
	return;
};

void draw_initial_cube(struct transfer_matrices *matrices, struct phong_lights *lights,
                       struct kemoview_shaders *kemo_shaders, struct VAO_ids *cube_VAO){
    float specular_tmp[4];
    float shiness_tmp[1];
    int nd;
    
    if(cube_VAO->npoint_draw <= 0) return;
    
    for(nd=0;nd<4;nd++){
        specular_tmp[nd] = lights->specular[nd];
        lights->specular[nd] = 1.0;
    };
    shiness_tmp[0] = lights->shiness[0];
    lights->shiness[0] = 100.0;
    
    glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
    set_phong_light_list(kemo_shaders->phong, lights);

	glBindVertexArray(cube_VAO->id_VAO);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glDrawElements(GL_TRIANGLES, cube_VAO->npoint_draw, GL_UNSIGNED_INT, 0);

    for(nd=0;nd<4;nd++){lights->specular[nd] = specular_tmp[nd];};
    lights->shiness[0] = shiness_tmp[0];
return;
}

void draw_cube_edge_gl3(struct view_element *view_s, 
						struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders){
    struct transfer_matrices *matrices;
    
	struct gl_strided_buffer *gl_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(8, gl_buf);
	alloc_strided_buffer(gl_buf);
    struct gl_index_buffer *index_buf = init_gl_index_buffer(12, 3);
    CubeNode_to_buf(0.5f, gl_buf, index_buf);
    free(index_buf->ie_buf);
    free(index_buf);

	glGenVertexArrays(1, &(cube_VAO->id_VAO));
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
	glDeleteVertexArrays(1, &(cube_VAO->id_VAO));
}

void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *quad_buf
			= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	
	set_buffer_address_4_patch(4, quad_buf);
	alloc_strided_buffer(quad_buf);
	
	glGenVertexArrays(1, &(quad_VAO->id_VAO));
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
	glDeleteVertexArrays(1, &(quad_VAO->id_VAO));
	
	free(quad_buf->v_buf);
	free(quad_buf);
	
	return;
};
