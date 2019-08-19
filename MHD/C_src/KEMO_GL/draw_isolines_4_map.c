/*
 *  draw_isolines_4_map.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include <OpenGL/gl3.h>
#include "draw_isolines_4_map.h"

void draw_map_PSF_isolines_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			int iflag_retina, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf){
	int inum_line = 0;
	int num_patch = 2 * count_map_PSF_isoline(psf_s, psf_m);
	if(num_patch <= 0) return;
	
	set_buffer_address_4_patch(ITHREE*num_patch, psf_buf);
	resize_strided_buffer(psf_buf->num_nod_buf, psf_buf->ncomp_buf, psf_buf);
	inum_line = set_map_PSF_isoline_to_buf(psf_s, psf_m, psf_buf);
	
	Const_VAO_4_Simple(psf_VAO, psf_buf);
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	glBindVertexArray(psf_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Simple_VAO(psf_VAO);
	
	return;
}

