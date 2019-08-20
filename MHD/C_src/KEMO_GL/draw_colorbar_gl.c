/*
// draw_colorbar_gl.c
*/

#include <OpenGL/gl3.h>
#include "draw_colorbar_gl.h"

static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};

void set_colorbar_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], 
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk,
			struct VAO_ids **cbar_VAO, struct gl_strided_buffer *cbar_buf){
	int num_patch;
	int inum_quad;
	
	
	set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win, cmap_s, cbar_wk);
	
	num_patch = 4 * cbar_wk->num_quad;
	num_patch = num_patch + 2*(cbar_wk->iflag_zero + IFOUR);
	cbar_VAO[0]->npoint_draw = ITHREE * num_patch;
	
	set_buffer_address_4_patch(cbar_VAO[0]->npoint_draw, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	inum_quad = 0;
	inum_quad = colorbar_frame_to_buf(inum_quad, iflag_retina, text_color, cbar_wk, cbar_buf);
	inum_quad = solid_colorbar_box_to_buf(inum_quad, cmap_s, cbar_wk, cbar_buf);
	inum_quad = fade_colorbar_box_to_buf(inum_quad, cmap_s, bg_color, cbar_wk, cbar_buf);
	
	glGenVertexArrays(1, &cbar_VAO[0]->id_VAO);
	glGenVertexArrays(1, &cbar_VAO[1]->id_VAO);
	
	
	
	glBindVertexArray(cbar_VAO[0]->id_VAO);
	Const_VAO_4_Simple(cbar_VAO[0], cbar_buf);
	glBindVertexArray(0);
	
	set_colorbar_text_image(text_color, cbar_wk);
	
	cbar_VAO[1]->npoint_draw = ITHREE*2*(cbar_wk->iflag_zero + ITWO);
	set_buffer_address_4_patch(cbar_VAO[1]->npoint_draw, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_mbox_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	glBindVertexArray(cbar_VAO[1]->id_VAO);
	
	glGenBuffers(1, &cbar_VAO[1]->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO[1]->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * cbar_buf->num_nod_buf*cbar_buf->ncomp_buf,
				 cbar_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, cbar_buf->istride,
						  (GLvoid*) (cbar_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, cbar_buf->istride, 
						  (GLvoid*) (cbar_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	cbar_wk->id_texture = set_texture_to_buffer(IWIDTH_TXT, 3*IHIGHT_TXT, cbar_wk->numBMP);
	glBindVertexArray(0);
	
	return;
};

void draw_colorbar_VAO(struct cbar_work *cbar_wk,
			struct VAO_ids **cbar_VAO, struct kemoview_shaders *kemo_shaders){
	GLdouble orthogonal[16];
	orthogonal_glmat_c(0.0, cbar_wk->xwin, 0.0, cbar_wk->ywin, -1.0, 1.0, orthogonal);
	
	glEnable(GL_BLEND);
	glEnable(GL_TRUE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_MULTISAMPLE);
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
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
	
	//	Destroy_Simple_VAO(cbar_VAO);
	//	DestroyVBO(cbar_VAO);
	
	
	return;
}

