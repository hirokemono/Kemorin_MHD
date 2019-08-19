/*
// draw_isolines_4_PSF.c
*/

#include <OpenGL/gl3.h>
#include "draw_isolines_4_PSF.h"

void draw_PSF_isoline_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int num_patch = 0;
	int inum = 0;
	int j, nd;
	double v_line;
	double f_color[4];
	
	num_patch = 2*count_PSF_all_isolines_to_buf(psf_s, psf_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	
	inum - set_PSF_all_isolines_to_buf(psf_s, psf_m, psf_buf);
	
	glUseProgram(kemo_shaders->phong->programId);
	transfer_matrix_to_shader(kemo_shaders->phong, view_s);
	set_phong_light_list(kemo_shaders->phong, kemo_shaders->lights);
	
	Const_VAO_4_Phong(psf_VAO, psf_buf);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Phong_VAO(psf_VAO);
	
	return;
}
