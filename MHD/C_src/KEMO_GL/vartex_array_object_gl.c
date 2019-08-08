/*
//  vartex_array_object_gl.c
//  
//
//  Created by Hiroaki Matsui on 2019/08/05.
*/

#include "vartex_array_object_gl.h"

void set_buffer_address_4_patch(struct gl_strided_buffer *strided_buf){
	strided_buf->ist_xyz =    0;
	strided_buf->ist_data =   3;
	strided_buf->ist_csurf =  4;
	strided_buf->ist_norm =   8;
	strided_buf->ist_tex =   11;
	
	strided_buf->ncomp_buf = 13;
	strided_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
	strided_buf->ntot = strided_buf->num_nod_buf * strided_buf->ncomp_buf;
	
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
	strided_buf->ntot = strided_buf->num_nod_buf * strided_buf->ncomp_buf;
	
	return;
};

struct gl_strided_buffer * init_buffer_for_gl(){
	struct gl_strided_buffer *strided_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	set_buffer_address_4_patch(strided_buf);
	return strided_buf;
};

void dealloc_buffer_for_gl(struct gl_strided_buffer *strided_buf){
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
	strided_buf->x_draw = &strided_buf->v_buf[strided_buf->istride*inum + strided_buf->ist_xyz];
	strided_buf->d_draw = &strided_buf->v_buf[strided_buf->istride*inum + strided_buf->ist_data];
	strided_buf->c_draw = &strided_buf->v_buf[strided_buf->istride*inum + strided_buf->ist_csurf];
	strided_buf->n_draw = &strided_buf->v_buf[strided_buf->istride*inum + strided_buf->ist_norm];
	strided_buf->x_txur = &strided_buf->v_buf[strided_buf->istride*inum + strided_buf->ist_tex];
	return;
};

void select_stride_VBO(int inum, struct gl_strided_buffer *strided_buf){
	if(strided_buf->istride == strided_buf->ncomp_buf){
		set_node_stride_VBO(inum, strided_buf);
	} else {
		set_zero_stride_VBO(inum, strided_buf);
	};
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
}
