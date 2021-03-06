
/* draw_menu_bottun_gl.c */
#include "draw_menu_bottun_gl.h"

void draw_menu_by_VAO(struct VAO_ids *menu_VAO, struct kemoview_shaders *kemo_shaders){
	struct gl_strided_buffer *strided_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(128, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	const_menu_bottun_buffer(strided_buf);
	
	GLenum ErrorCheckValue = glGetError();
	
	Const_VAO_4_Simple(menu_VAO, strided_buf);
	free(strided_buf->v_buf);
	free(strided_buf);
	
	glUseProgram(kemo_shaders->menu->programId);
	identity_matrix_to_shader(kemo_shaders->menu);
	
	glBindVertexArray(menu_VAO->id_VAO);
	glDrawArrays(GL_POINTS, 0, MENU_HEIGHT*MENU_WIDTH);
	return;
}

