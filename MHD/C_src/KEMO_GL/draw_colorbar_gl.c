/*
// draw_colorbar_gl.c
*/

#include <OpenGL/gl3.h>
#include "draw_colorbar_gl.h"

static const GLfloat black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};

void draw_colorbar_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], 
			struct colormap_params *cmap_s, struct cbar_work *cbar_wk,
			struct VAO_ids *cbar_VAO, struct kemoview_shaders *kemo_shaders,
			struct gl_strided_buffer *cbar_buf){
	int num_patch;
	
	GLdouble orthogonal[16];
	
	set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win, cmap_s, cbar_wk);
	orthogonal_glmat_c(0.0, cbar_wk->xwin, 0.0, cbar_wk->ywin, -1.0, 1.0, orthogonal);
	
	if(cmap_s->min_opacity < 1.0) {
		glEnable(GL_BLEND);
		glEnable(GL_TRUE);
		glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
		glEnable(GL_MULTISAMPLE);
	}
	
	
	glUseProgram(kemo_shaders->test->programId);
	map_matrix_to_shader(kemo_shaders->test, orthogonal);
	
	num_patch = 4 * cbar_wk->num_quad;
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	solid_colorbar_box_to_buf(cmap_s, cbar_wk, cbar_buf);
	fade_colorbar_box_to_buf(cbar_wk->num_quad, cmap_s, bg_color, cbar_wk, cbar_buf);
	
	Const_VAO_4_Simple(cbar_VAO, cbar_buf);
	
	glEnable(GL_BLEND);
	glEnable(GL_TRUE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_MULTISAMPLE);
	
	glBindVertexArray(cbar_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	Destroy_Simple_VAO(cbar_VAO);
	
	num_patch = 2*(cbar_wk->iflag_zero + IFOUR);
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_frame_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	Const_VAO_4_Simple(cbar_VAO, cbar_buf);
	
	glBindVertexArray(cbar_VAO->id_VAO);
	glDrawArrays(GL_TRIANGLES, IZERO, (ITHREE*num_patch));
	
	Destroy_Simple_VAO(cbar_VAO);
	
	set_colorbar_text_image(text_color, cbar_wk);
	
	glUseProgram(kemo_shaders->simple_texure->programId);
	map_matrix_to_shader(kemo_shaders->simple_texure, orthogonal);
	
	int id_textureImage = glGetUniformLocation(kemo_shaders->simple_texure->programId, "image");
	
	GLuint id_texture[1];
	static const GLfloat blend[] = {1.0,1.0,1.0,1.0};
	/* Preference for resiging texture */
	glBindTexture(GL_TEXTURE_2D , id_texture[0]);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(GL_TEXTURE_2D , 0 , GL_RGBA , IWIDTH_TXT, 3*IHIGHT_TXT,
				 0 , GL_RGBA , GL_UNSIGNED_BYTE , cbar_wk->numBMP);
	//	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
	//	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, blend);

	
	
	num_patch = 2*(cbar_wk->iflag_zero + ITWO);
	set_buffer_address_4_patch(ITHREE*num_patch, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	colorbar_mbox_to_buf(iflag_retina, text_color, cbar_wk, cbar_buf);
	
	glGenVertexArrays(1, &cbar_VAO->id_VAO);
	glBindVertexArray(cbar_VAO->id_VAO);
	
	glGenBuffers(1, &cbar_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * cbar_buf->num_nod_buf*cbar_buf->ncomp_buf,
				 cbar_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, cbar_buf->istride,
						  (GLvoid*) (cbar_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, cbar_buf->istride, 
						  (GLvoid*) (cbar_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);

	glBindVertexArray(cbar_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, cbar_VAO->id_vertex);
	
	glUniform1i(id_textureImage, id_texture[0]);
	glDrawArrays(GL_TRIANGLES, 0, 12);
	
	if(cbar_wk->iflag_zero == 1){
		glDrawArrays(GL_TRIANGLES, 12, 6);
	};
	
	DestroyVBO(cbar_VAO);
	
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);
	
	return;
}

