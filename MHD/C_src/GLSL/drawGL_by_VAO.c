
/* drawGL_by_VAO.c */


#include "drawGL_by_VAO.h"

void drawgl_textured_patches_VAO(GLuint *texture_name, 
                                 struct transfer_matrices *matrices,
                                 struct phong_lights *lights, 
                                 struct kemoview_shaders *kemo_shaders,
                                 struct VAO_ids *VAO){
	if(VAO->npoint_draw <= 0) return;
	
	glUseProgram(kemo_shaders->phong_texure->programId);
    transfer_matrix_to_GL(kemo_shaders->phong_texure, matrices);
	set_phong_light_list(kemo_shaders->phong_texure, lights);

	glBindVertexArray(VAO->id_VAO);
	
	glBindTexture(GL_TEXTURE_2D, *texture_name);
	int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
	glUniform1i(id_textureImage, 0);
	
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glDrawArrays(GL_TRIANGLES, IZERO, VAO->npoint_draw);
	return;
};

void drawgl_patch_with_phong(struct transfer_matrices *matrices, struct phong_lights *lights, 
                             struct kemoview_shaders *kemo_shaders, struct VAO_ids *VAO){
	if(VAO->npoint_draw <= 0) return;
	
	glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
	set_phong_light_list(kemo_shaders->phong, lights);

	glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, VAO->npoint_draw);
	
	return;
}



void drawgl_elements_with_phong(struct transfer_matrices *matrices, struct phong_lights *lights,
                                struct kemoview_shaders *kemo_shaders, struct VAO_ids *VAO){
    if(VAO->npoint_draw <= 0) return;
    
    glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
    set_phong_light_list(kemo_shaders->phong, lights);

    glBindVertexArray(VAO->id_VAO);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glDrawElements(GL_TRIANGLES, VAO->npoint_draw, GL_UNSIGNED_INT, IZERO);
    return;
}

void drawgl_textured_elements_VAO(GLuint *texture_name,
                                  struct transfer_matrices *matrices,
                                  struct phong_lights *lights,
                                  struct kemoview_shaders *kemo_shaders,
                                  struct VAO_ids *VAO){
    if(VAO->npoint_draw <= 0) return;
    
    glUseProgram(kemo_shaders->phong_texure->programId);
    transfer_matrix_to_GL(kemo_shaders->phong_texure, matrices);
    set_phong_light_list(kemo_shaders->phong_texure, lights);

    glBindVertexArray(VAO->id_VAO);
    
    glBindTexture(GL_TEXTURE_2D, *texture_name);
    int id_textureImage = glGetUniformLocation(kemo_shaders->phong_texure->programId, "image");
    glUniform1i(id_textureImage, 0);
    
    glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO->id_index);
    glDrawElements(GL_TRIANGLES, VAO->npoint_draw , GL_UNSIGNED_INT, 0);
    return;
};



void drawgl_points(struct transfer_matrices *matrices, struct VAO_ids *VAO,
                   struct kemoview_shaders *kemo_shaders){
    if(VAO->npoint_draw <= 0) return;
    
    glDisable(GL_CULL_FACE);
    glUseProgram(kemo_shaders->simple->programId);
    transfer_matrix_to_GL(kemo_shaders->simple, matrices);

    glBindVertexArray(VAO->id_VAO);
    glDrawArrays(GL_POINTS, IZERO, VAO->npoint_draw);
    return;
};

void drawgl_lines(struct transfer_matrices *matrices, struct VAO_ids *VAO,
                  struct kemoview_shaders *kemo_shaders){
	if(VAO->npoint_draw <= 0) return;
	
	glDisable(GL_CULL_FACE);
	glUseProgram(kemo_shaders->simple->programId);
    transfer_matrix_to_GL(kemo_shaders->simple, matrices);

    glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, VAO->npoint_draw);
	return;
};

void draw_map_objects_VAO(struct transfer_matrices *matrices,
                          struct VAO_ids **map_VAO, struct VAO_ids **map_index_VAO,
                          struct kemoview_shaders *kemo_shaders){
	int i;
    /* set shading mode */
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glUseProgram(kemo_shaders->simple->programId);

    map_matrix_to_GLSL(kemo_shaders->simple, matrices);

    for(i=0;i<3;i++){
        if(map_index_VAO[i]->npoint_draw > 0){
            glBindVertexArray(map_index_VAO[i]->id_VAO);
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, map_index_VAO[i]->id_index);
            glDrawElements(GL_TRIANGLES, map_index_VAO[i]->npoint_draw , GL_UNSIGNED_INT, 0);
        }
    };
    
	for(i=0;i<1;i++){
		if(map_VAO[i]->npoint_draw > 0){
			glBindVertexArray(map_VAO[i]->id_VAO);
			glDrawArrays(GL_TRIANGLES, IZERO, map_VAO[i]->npoint_draw);
		};
	};	
	return;
}


void draw_solid_mesh_VAO(int polygon_mode, struct transfer_matrices *matrices, 
                         struct phong_lights *lights, struct VAO_ids *mesh_solid_VAO, 
                         struct kemoview_shaders *kemo_shaders){
	if(mesh_solid_VAO->npoint_draw <= 0) return;
	
	glEnable(GL_CULL_FACE);
	if(polygon_mode == NORMAL_POLYGON) { 
		glPolygonMode(GL_FRONT, GL_FILL);
		glCullFace(GL_BACK);
	} else if(polygon_mode == REVERSE_POLYGON) {
		glPolygonMode(GL_BACK, GL_FILL);
		glCullFace(GL_FRONT);
	};
	
	glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
	set_phong_light_list(kemo_shaders->phong, lights);

	glBindVertexArray(mesh_solid_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (mesh_solid_VAO->npoint_draw));
    glCullFace(GL_FRONT);
	return;
};

void draw_trans_mesh_VAO(struct transfer_matrices *matrices, 
                         struct phong_lights *lights, 
                         struct VAO_ids *mesh_trans_VAO,
                         struct kemoview_shaders *kemo_shaders){
	if(mesh_trans_VAO->npoint_draw <= 0) return;
	
	glDisable(GL_CULL_FACE);
	glDepthMask(GL_FALSE);
    glEnable(GL_MULTISAMPLE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	glUseProgram(kemo_shaders->phong->programId);
    transfer_matrix_to_GL(kemo_shaders->phong, matrices);
	set_phong_light_list(kemo_shaders->phong, lights);
	
	glBindVertexArray(mesh_trans_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (mesh_trans_VAO->npoint_draw));
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
	return;
}

void draw_2D_box_patch_VAO(struct transfer_matrices *matrices, struct VAO_ids *VAO,
						   struct kemoview_shaders *kemo_shaders){
    if(VAO->npoint_draw <= 0) return;
    
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    
	glUseProgram(kemo_shaders->simple->programId);
    map_matrix_to_GLSL(kemo_shaders->simple, matrices);

	glBindVertexArray(VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, VAO->npoint_draw);

    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    return;
}

void draw_textured_2D_box_VAO(struct transfer_matrices *matrices, struct VAO_ids *VAO,
                              struct kemoview_shaders *kemo_shaders){
    if(VAO->npoint_draw <= 0) return;
    
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glEnable(GL_MULTISAMPLE);
    glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    
    glUseProgram(kemo_shaders->simple_texure->programId);
    map_matrix_to_GLSL(kemo_shaders->simple_texure, matrices);

    glBindVertexArray(VAO->id_VAO);

    glBindTexture(GL_TEXTURE_2D, VAO->id_texure);
    int id_textureImage = glGetUniformLocation(kemo_shaders->simple_texure->programId, "image");
    glUniform1i(id_textureImage, 0);
    
    glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
    glDrawArrays(GL_TRIANGLES, 0, VAO->npoint_draw);
    
    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
    glDisable(GL_MULTISAMPLE);
    return;
}
