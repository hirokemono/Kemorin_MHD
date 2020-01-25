/*
// draw_messages_gl.c
*/

#include "draw_messages_gl.h"

static void count_message_text_VAO(struct VAO_ids *text_VAO){
	text_VAO->npoint_draw = ITHREE*2;
	return;
};

static void set_message_text_VAO(int iflag_retina, 
								 GLfloat text_color[4], GLfloat bg_color[4], 
								 struct msg_work *msg_wk, struct VAO_ids *text_VAO,
								 struct gl_strided_buffer *cbar_buf){
	set_buffer_address_4_patch(text_VAO->npoint_draw, cbar_buf);
	resize_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
	
	message_mbox_to_buf(iflag_retina, text_color, msg_wk, cbar_buf);
	
	glBindVertexArray(text_VAO->id_VAO);
	Const_VAO_4_Texture(text_VAO, cbar_buf);
	msg_wk->id_texture = set_texture_to_buffer(IWIDTH_TXT, 3*IHIGHT_TXT, msg_wk->numBMP);
	glBindVertexArray(0);
	return;
};

void set_message_VAO(int iflag_retina, GLint nx_win, GLint ny_win,
			GLfloat text_color[4], GLfloat bg_color[4], struct msg_work *msg_wk, 
			struct VAO_ids *msg_VAO){
	struct gl_strided_buffer *cbar_buf 
		= (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(16, cbar_buf);
	alloc_strided_buffer(cbar_buf->num_nod_buf, cbar_buf->ncomp_buf, cbar_buf);
		
	msg_VAO->npoint_draw = 0;
	clear_message_text_image(msg_wk);
	
	set_message_position(iflag_retina, (int) nx_win, (int) ny_win, msg_wk);

	set_windowsize_image((int) nx_win, (int) ny_win, text_color, msg_wk);
	
	count_message_text_VAO(msg_VAO);
	set_message_text_VAO(iflag_retina, text_color, bg_color, 
						 msg_wk, msg_VAO, cbar_buf);
	
	free(cbar_buf->v_buf);
	free(cbar_buf);
	return;
};

void draw_message_VAO(struct msg_work *msg_wk, 
			struct VAO_ids *msg_VAO, struct kemoview_shaders *kemo_shaders){
	double orthogonal[16];
	if(msg_VAO->npoint_draw <= 0) return;
	
	orthogonal_glmat_c(0.0, msg_wk->xwin, 0.0, msg_wk->ywin, -1.0, 1.0, orthogonal);
	
	glEnable(GL_BLEND);
	glEnable(GL_TRUE);
	glEnable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glEnable(GL_MULTISAMPLE);
	
	glUseProgram(kemo_shaders->simple_texure->programId);
	map_matrix_to_shader(kemo_shaders->simple_texure, orthogonal);
	
	glBindVertexArray(msg_VAO->id_VAO);
	glBindBuffer(GL_ARRAY_BUFFER, msg_VAO->id_vertex);
	
	glBindTexture(GL_TEXTURE_2D, msg_wk->id_texture);
	int id_textureImage = glGetUniformLocation(kemo_shaders->simple_texure->programId, "image");
	glUniform1i(id_textureImage, 0);
	
	glDrawArrays(GL_TRIANGLES, 0, msg_VAO->npoint_draw);
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
	glDisable(GL_SAMPLE_ALPHA_TO_COVERAGE);
	glDisable(GL_MULTISAMPLE);	
	
	return;
}

