
/* draw_menu_bottun_gl.c */
#include <OpenGL/gl3.h>
#include "draw_menu_bottun_gl.h"

void draw_menubottun_gl(struct VAO_ids *menu_VAO){
	GLubyte menubottun_bits[3*MENU_HEIGHT*MENU_WIDTH];
	
	menubottun_bitmap(menubottun_bits);
	kemoview_indentity_projectionmatrix();
	
	glClearColor(0.3, 0.9, 0.9, 1.0);
	glClear(GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT);
	glRasterPos2i(-1 , -1);
	glDrawPixels(MENU_WIDTH , MENU_HEIGHT , GL_RGB , GL_UNSIGNED_BYTE , menubottun_bits);
	
	glFlush();
	return;
}

void VBO_for_Menu(struct VAO_ids *menu_VAO, struct kemoview_shaders *kemo_shaders){
	GLfloat Vertices[4*MENU_WIDTH*MENU_HEIGHT];
	GLfloat Colors[4*MENU_WIDTH*MENU_HEIGHT];
	int i, j, idx;
	
	struct gl_strided_buffer *strided_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(128, strided_buf);
	alloc_strided_buffer(strided_buf->num_nod_buf, strided_buf->ncomp_buf, strided_buf);
	const_menu_bottun_buffer(strided_buf);
	
	glUseProgram(kemo_shaders->menu->programId);
	identity_matrix_to_shader(kemo_shaders->menu);
	
	GLenum ErrorCheckValue = glGetError();
	
	glGenVertexArrays(1, &menu_VAO->id_VAO);
	glBindVertexArray(menu_VAO->id_VAO);
	
	glGenBuffers(1, &menu_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, menu_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	free(strided_buf->v_buf);
	free(strided_buf);
	
	glBindVertexArray(menu_VAO->id_VAO);
	glDrawArrays(GL_POINTS, 0, MENU_HEIGHT*MENU_WIDTH);
	DestroyVBO(menu_VAO);
	return;
}

