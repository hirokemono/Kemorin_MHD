
/* draw_fieldlines.c */

#include <OpenGL/gl3.h>
#include "draw_fieldlines.h"

void draw_fieldtubes_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct view_element *view_s, 
			struct VAO_ids *fline_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *fline_buf){
	int ncorner = ISIX;
	int icou;
	
	int num_patch = count_fieldtubes_to_buf(ncorner, fline_s);
	if(num_patch <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	set_buffer_address_4_patch(ITHREE*num_patch, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	icou = set_fieldtubes_to_buf(ncorner, fline_s, fline_m, fline_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	glGenVertexArrays(1, &fline_VAO->id_VAO);
	glBindVertexArray(fline_VAO->id_VAO);
	Const_VAO_4_Phong(fline_VAO, fline_buf);
	glBindVertexArray(0);
	
	glBindVertexArray(fline_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(fline_VAO);
	
	return;
}

void draw_fieldlines_VAO(struct psf_data *fline_s, struct fline_menu_val *fline_m,
			struct view_element *view_s, 
			struct VAO_ids *fline_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *fline_buf){
	int inod, iele, k;
	int num, icou, inum;
	
	int num_edge = count_fieldlines_to_buf(fline_s);
	if(num_edge <= 0) return;
	
	set_color_code_for_fieldlines(fline_s, fline_m);
	
	glUseProgram(kemo_shaders->test->programId);
	transfer_matrix_to_shader(kemo_shaders->test, view_s);
	
	set_buffer_address_4_patch(ITWO*num_edge, fline_buf);
	resize_strided_buffer(fline_buf->num_nod_buf, fline_buf->ncomp_buf, fline_buf);
	
	icou = set_fieldlines_to_buf(fline_s, fline_m, fline_buf);
	
	glGenVertexArrays(1, &fline_VAO->id_VAO);
	glBindVertexArray(fline_VAO->id_VAO);
	Const_VAO_4_Simple(fline_VAO, fline_buf);
	glBindVertexArray(0);
	
	glBindVertexArray(fline_VAO->id_VAO);
	glDrawArrays(GL_LINES, IZERO, (ITWO*num_edge));
	Destroy_Simple_VAO(fline_VAO);
	
	return;
}
