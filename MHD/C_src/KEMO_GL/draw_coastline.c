
/* draw_coastline.c */


#include <OpenGL/gl3.h>
#include  "draw_coastline.h"


void draw_coastline_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int icou;
	int nedge_coast = count_coastline_buf();
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	
	set_buffer_address_4_patch(ITWO*nedge_coast, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	icou = set_coastline_buf(radius, line_buf);
	
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
	
	glGenVertexArrays(1, &line_VAO->id_VAO);
	glBindVertexArray(line_VAO->id_VAO);
	
	glGenBuffers(1, &line_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * line_buf->num_nod_buf*line_buf->ncomp_buf,
				 line_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, line_buf->istride,
						  (GLvoid*) (line_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindVertexArray(0);
	
	glBindVertexArray(line_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_coast));
	
	DestroyVBO(line_VAO);
	
	return;
};

void draw_map_coastline_VBO(const GLdouble *orthogonal, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int icou;
	int nedge_coast = count_coastline_buf();
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	set_buffer_address_4_patch(ITWO*nedge_coast, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	icou = set_map_coastline_buf(line_buf);
	
	glGenVertexArrays(1, &line_VAO->id_VAO);
	glBindVertexArray(line_VAO->id_VAO);
	
	glGenBuffers(1, &line_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * line_buf->num_nod_buf*line_buf->ncomp_buf,
				 line_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, line_buf->istride,
						  (GLvoid*) (line_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, line_buf->istride, 
						  (GLvoid*) (line_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	glBindVertexArray(line_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, line_VAO->id_vertex);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_coast));
	
	DestroyVBO(line_VAO);
	
	return;
};
