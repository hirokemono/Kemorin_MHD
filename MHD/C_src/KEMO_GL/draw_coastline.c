
/* draw_coastline.c */


#include <OpenGL/gl3.h>
#include  "draw_coastline.h"

void draw_sph_flame_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int nedge_flame = count_sph_flame();
	
	set_buffer_address_4_patch(ITWO*nedge_flame, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_sph_flame_to_buf(radius, line_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(line_VAO, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_flame));
	Destroy_Phong_VAO(line_VAO);
	
	return;
};

void draw_map_flame_VBO(const GLdouble *orthogonal, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int nedge_flame = count_sph_flame();
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	set_buffer_address_4_patch(ITWO*nedge_flame, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	set_map_flame_to_buf(line_buf);
	
	Const_VAO_4_Simple(line_VAO, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_flame));
	
	Destroy_Simple_VAO(line_VAO);
	
	return;
};


void draw_coastline_VBO(double radius, struct view_element *view_s, 
			struct VAO_ids *line_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *line_buf){
	int icou;
	int nedge_coast = count_coastline_buf();
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	set_buffer_address_4_patch(ITWO*nedge_coast, line_buf);
	resize_strided_buffer(line_buf->num_nod_buf, line_buf->ncomp_buf, line_buf);
	icou = set_coastline_buf(radius, line_buf);
	
	Const_VAO_4_Phong(line_VAO, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_coast));
	Destroy_Phong_VAO(line_VAO);
	
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
	
	Const_VAO_4_Simple(line_VAO, line_buf);
	
	glBindVertexArray(line_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*nedge_coast));
	Destroy_Simple_VAO(line_VAO);
	
	return;
};
