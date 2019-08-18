/*
//  vartex_array_object_gl.c
//  
//
//  Created by Hiroaki Matsui on 2019/08/05.
*/

#include "vartex_array_object_gl.h"

void set_buffer_address_4_patch(int num_points, struct gl_strided_buffer *strided_buf){
	strided_buf->ist_xyz =    0;
	strided_buf->ist_data =   3;
	strided_buf->ist_csurf =  4;
	strided_buf->ist_norm =   8;
	strided_buf->ist_tex =   11;
	
	strided_buf->ncomp_buf = 13;
	strided_buf->num_nod_buf = num_points;
	
	return;
};

void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf){
	strided_buf->ist_tex =   0;
	strided_buf->ist_data =   2;
	strided_buf->ist_csurf =  3;
	
	strided_buf->ist_norm =   0;
	strided_buf->ist_xyz =    0;
	
	strided_buf->ncomp_buf = 7;
	strided_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
	
	return;
};

void alloc_strided_buffer(int num_points, int num_comp, 
			struct gl_strided_buffer *strided_buf){
	strided_buf->num_nod_buf = num_points;
	strided_buf->ncomp_buf = num_comp;
	strided_buf->istride = sizeof(GLfloat) * strided_buf->ncomp_buf;
	
	strided_buf->nsize_buf = strided_buf->num_nod_buf * strided_buf->ncomp_buf;
	if((strided_buf->v_buf = (GLfloat *) malloc(strided_buf->nsize_buf*sizeof(GLfloat))) == NULL){
        printf("malloc error for strided_buf->v_buf\n");
        exit(0);
	};
};

struct gl_strided_buffer * init_strided_buffer(int num_points){
	struct gl_strided_buffer *strided_buf;
	if((strided_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer))) == NULL){
        printf("malloc error for gl_strided_buffer\n");
        exit(0);
	};
	
	set_buffer_address_4_patch(4 * NPATCH_GL_BUFFER, strided_buf);
	alloc_strided_buffer((4*NPATCH_GL_BUFFER), 16, strided_buf);
	return strided_buf;
};

void resize_strided_buffer(int num_points, int num_comp, 
			struct gl_strided_buffer *strided_buf){
	strided_buf->num_nod_buf = num_points;
	strided_buf->ncomp_buf = num_comp;
	strided_buf->istride = sizeof(GLfloat) * strided_buf->ncomp_buf;
	
	if(num_points*num_comp <= strided_buf->nsize_buf) return;
	
	GLfloat *tmp = NULL;
	strided_buf->nsize_buf = strided_buf->num_nod_buf * strided_buf->ncomp_buf;
	tmp = (GLfloat *) realloc(strided_buf->v_buf, strided_buf->nsize_buf*sizeof(GLfloat));
	if(tmp == NULL){
        printf("reallocation error for strided_buf->v_buf\n");
		exit(-1);
	} else {
		strided_buf->v_buf = tmp;
	};
	
	return;
};


void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf){
	free(strided_buf->v_buf);
	free(strided_buf);
	return;
};

void set_zero_stride_VBO(int inum, struct gl_strided_buffer *strided_buf){
	strided_buf->x_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_xyz*strided_buf->num_nod_buf];
	strided_buf->d_draw = &strided_buf->v_buf[inum +   strided_buf->ist_data*strided_buf->num_nod_buf];
	strided_buf->c_draw = &strided_buf->v_buf[4*inum + strided_buf->ist_csurf*strided_buf->num_nod_buf];
	strided_buf->n_draw = &strided_buf->v_buf[3*inum + strided_buf->ist_norm*strided_buf->num_nod_buf];
	strided_buf->x_txur = &strided_buf->v_buf[2*inum + strided_buf->ist_tex*strided_buf->num_nod_buf];
	return;
};

void set_node_stride_VBO(int inum, struct gl_strided_buffer *strided_buf){
	strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_xyz];
	strided_buf->d_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_data];
	strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_csurf];
	strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_norm];
	strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ncomp_buf*inum + strided_buf->ist_tex];
	return;
};

void select_stride_VBO(int inum, struct gl_strided_buffer *strided_buf){
	if(strided_buf->istride == 0){
		set_zero_stride_VBO(inum, strided_buf);
	} else {
		set_node_stride_VBO(inum, strided_buf);
	};
}


void DestroyVBO_4_test(struct VAO_ids *VAO)
{
	GLenum ErrorCheckValue = glGetError();
	
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(0);
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	glDeleteBuffers(1, &VAO->id_color);
	glDeleteBuffers(1, &VAO->id_vertex);
	
	glBindVertexArray(0);
	glDeleteVertexArrays(1, &VAO->id_VAO);
	/*
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not destroy the VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	*/
}

void DestroyVBO(struct VAO_ids *VAO)
{
	GLenum ErrorCheckValue = glGetError();
	
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(0);
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	glDeleteBuffers(1, &VAO->id_color);
	glDeleteBuffers(1, &VAO->id_vertex);
	
	glBindVertexArray(0);
	glDeleteVertexArrays(1, &VAO->id_VAO);
	/*
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not destroy the VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	*/
}
