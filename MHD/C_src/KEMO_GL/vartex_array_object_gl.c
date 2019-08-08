/*
//  vartex_array_object_gl.c
//  
//
//  Created by Hiroaki Matsui on 2019/08/05.
*/

#include "vartex_array_object_gl.h"

struct gl_strided_buffer * init_buffer_for_gl(){
	struct gl_strided_buffer *gl_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
	
	gl_buf->ist_xyz =    0;
	gl_buf->ist_data =   3;
	gl_buf->ist_csurf =  4;
	gl_buf->ist_norm =   8;
	gl_buf->ist_tex =   11;
	
	gl_buf->ncomp_buf = 13;
	gl_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
	gl_buf->ntot = gl_buf->num_nod_buf * gl_buf->ncomp_buf;

	return gl_buf;
};

void dealloc_buffer_for_gl(struct gl_strided_buffer *gl_buf){
	free(gl_buf->v_buf);
	free(gl_buf);
	return;
};

void set_buffer_address_4_patch(struct gl_strided_buffer *gl_buf){
	gl_buf->ist_xyz =    0;
	gl_buf->ist_data =   3;
	gl_buf->ist_csurf =  4;
	gl_buf->ist_norm =   8;
	gl_buf->ist_tex =   11;
	
	gl_buf->ncomp_buf = 13;
	gl_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
	gl_buf->ntot = gl_buf->num_nod_buf * gl_buf->ncomp_buf;
	
	return;
};

void set_buffer_address_4_map(struct gl_strided_buffer *gl_buf){
	gl_buf->ist_tex =   0;
	gl_buf->ist_data =   2;
	gl_buf->ist_csurf =  3;
	
	gl_buf->ist_norm =   0;
	gl_buf->ist_xyz =    0;
	
	gl_buf->ncomp_buf = 7;
	gl_buf->num_nod_buf = 4 * NPATCH_GL_BUFFER;
	gl_buf->ntot = gl_buf->num_nod_buf * gl_buf->ncomp_buf;
	
	return;
};

void set_zero_stride_VBO(int inum, struct gl_strided_buffer *gl_buf){
	gl_buf->x_draw = &gl_buf->v_buf[3*inum + gl_buf->ist_xyz*gl_buf->num_nod_buf];
	gl_buf->d_draw = &gl_buf->v_buf[inum +   gl_buf->ist_data*gl_buf->num_nod_buf];
	gl_buf->c_draw = &gl_buf->v_buf[4*inum + gl_buf->ist_csurf*gl_buf->num_nod_buf];
	gl_buf->n_draw = &gl_buf->v_buf[3*inum + gl_buf->ist_norm*gl_buf->num_nod_buf];
	gl_buf->x_txur = &gl_buf->v_buf[2*inum + gl_buf->ist_tex*gl_buf->num_nod_buf];
	return;
};

void set_node_stride_VBO(int inum, struct gl_strided_buffer *gl_buf){
	gl_buf->x_draw =  &gl_buf->v_buf[gl_buf->istride*inum + gl_buf->ist_xyz];
	gl_buf->d_draw = &gl_buf->v_buf[gl_buf->istride*inum + gl_buf->ist_data];
	gl_buf->c_draw = &gl_buf->v_buf[gl_buf->istride*inum + gl_buf->ist_csurf];
	gl_buf->n_draw = &gl_buf->v_buf[gl_buf->istride*inum + gl_buf->ist_norm];
	gl_buf->x_txur =   &gl_buf->v_buf[gl_buf->istride*inum + gl_buf->ist_tex];
	return;
};

void select_stride_VBO(int inum, struct gl_strided_buffer *gl_buf){
	if(gl_buf->istride == gl_buf->ncomp_buf){
		set_node_stride_VBO(inum, gl_buf);
	} else {
		set_zero_stride_VBO(inum, gl_buf);
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
