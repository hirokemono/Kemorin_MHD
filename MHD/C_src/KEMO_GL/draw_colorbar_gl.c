/*
// draw_colorbar_gl.c
*/

#include "draw_colorbar_gl.h"

static void count_colorbar_box_VAO(struct cbar_work *cbar_wk, struct VAO_ids *cbar_VAO){
	int num_patch;
	
	num_patch = 4 * cbar_wk->num_quad;
	num_patch = num_patch + 2*(cbar_wk->iflag_zero + IFOUR);
	cbar_VAO->npoint_draw = ITHREE * num_patch;
	return;
};

static void count_colorbar_text_VAO(struct cbar_work *cbar_wk,struct VAO_ids *text_VAO){
	text_VAO->npoint_draw = ITHREE*2*(cbar_wk->iflag_zero + ITWO);
	return;
};

static void set_colorbar_box_VAO(int iflag_retina, GLfloat text_color[4], GLfloat bg_color[4], 
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk,
			struct VAO_ids *cbar_VAO, struct gl_strided_buffer *cbar_buf){
	int inum_quad;
	set_buffer_address_4_patch(cbar_VAO->npoint_draw, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	inum_quad = 0;
	inum_quad = solid_colorbar_box_to_buf(inum_quad, cmap_s, cbar_wk, cbar_buf);
	inum_quad = fade_colorbar_box_to_buf(inum_quad, cmap_s, bg_color, cbar_wk, cbar_buf);
	inum_quad = colorbar_frame_to_buf(inum_quad, iflag_retina, text_color, cbar_wk, cbar_buf);
	
	Const_VAO_4_Simple(cbar_VAO, cbar_buf);
	return;
};

static void set_colorbar_text_VAO(int iflag_retina, 
								  GLfloat text_color[4], GLfloat bg_color[4], 
								  struct cbar_work *cbar_wk, struct VAO_ids *text_VAO,
								  struct gl_strided_buffer *cbar_buf){
	set_buffer_address_4_patch(text_VAO->npoint_draw, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_mbox_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
	cbar_wk->id_texture = set_texture_to_buffer(cbar_wk->npix_x, 3*cbar_wk->npix_y,
                                                cbar_wk->numBMP);
	glBindVertexArray(0);
	return;
};

void set_colorbar_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4],
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct VAO_ids **cbar_VAO){
	int i;
	int icomp;
	struct gl_strided_buffer *cbar_buf 
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(16, cbar_buf);
	alloc_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
		
	cbar_VAO[1]->npoint_draw = 0;
	clear_colorbar_text_image(psf_a->cbar_wk);
	for(i=0; i<psf_a->nmax_loaded; i++){
		if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0) {
			icomp = psf_m[i]->icomp_draw_psf;
			set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win, 
						psf_m[i]->cmap_psf_comp[icomp], psf_a->cbar_wk);
			set_colorbar_text_image(text_color, psf_a->cbar_wk);
	
			count_colorbar_box_VAO(psf_a->cbar_wk, cbar_VAO[0]);
			count_colorbar_text_VAO(psf_a->cbar_wk, cbar_VAO[1]);
			set_colorbar_box_VAO(iflag_retina, text_color, bg_color, 
						psf_m[i]->cmap_psf_comp[icomp], psf_a->cbar_wk, cbar_VAO[0], cbar_buf);
			set_colorbar_text_VAO(iflag_retina, text_color, bg_color, 
						psf_a->cbar_wk, cbar_VAO[1], cbar_buf);
		};
	};
	free(cbar_buf->v_buf);
	free(cbar_buf);
	return;
};

void draw_colorbar_VAO(struct cbar_work *cbar_wk,
			struct VAO_ids **cbar_VAO, struct kemoview_shaders *kemo_shaders){
	double orthogonal[16];
	if(cbar_VAO[1]->npoint_draw <= 0) return;
	
	orthogonal_glmat_c(0.0, cbar_wk->xwin, 0.0, cbar_wk->ywin, -1.0, 1.0, orthogonal);
	
	glEnable(GL_BLEND);
	glEnable(GL_TRUE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_MULTISAMPLE);
	
	glUseProgram(kemo_shaders->simple->programId);
	map_matrix_to_shader(kemo_shaders->simple, orthogonal);
	
	glBindVertexArray(cbar_VAO[0]->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, cbar_VAO[0]->npoint_draw);
	
	
	glUseProgram(kemo_shaders->simple_texure->programId);
	map_matrix_to_shader(kemo_shaders->simple_texure, orthogonal);
	
	glBindVertexArray(cbar_VAO[1]->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO[1]->id_vertex);
	
	glBindTexture(GL_TEXTURE_2D, cbar_wk->id_texture);
	int id_textureImage = glGetUniformLocation(kemo_shaders->simple_texure->programId, "image");
	glUniform1i(id_textureImage, 0);
	
	glDrawArrays(GL_TRIANGLES, 0, cbar_VAO[1]->npoint_draw);
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);	
	
	return;
}

