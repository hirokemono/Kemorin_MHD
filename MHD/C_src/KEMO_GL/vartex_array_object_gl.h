/*
//  vartex_array_object_gl.h
//  
//
//  Created by Hiroaki Matsui on 2019/08/05.
*/

#ifndef vartex_array_object_gl_h__
#define vartex_array_object_gl_h__

#include <stdlib.h>
#include <stdio.h>
#include "kemoviewer.h"

#define NPATCH_GL_BUFFER  4096
#define NSIZE_GL_BUFFER  32768

struct buffer_for_gl{
	GLfloat xyz[4*NSIZE_GL_BUFFER][3];
	GLfloat xy[4*NSIZE_GL_BUFFER][2];
	GLfloat norm[4*NSIZE_GL_BUFFER][3];
	GLfloat rgba[4*NSIZE_GL_BUFFER][4];
};

struct gl_strided_buffer{
	int nsize_buf;
	int istride;
	
	int ncomp_buf;
	int num_nod_buf;
	int ntot;
	
	int ist_xyz;
	int ist_rtp;
	int ist_norm;
	int ist_tex;
	int ist_csurf;
	int ist_data;
	
	GLfloat *v_buf;
	
	GLfloat *x_draw;
	GLfloat *x_txur;
	GLfloat *n_draw;
	GLfloat *c_draw;
	GLfloat *d_draw;
};

struct VAO_ids{
	GLuint id_VAO;
	
	GLuint id_vertex;
	GLuint id_color;
	
	GLuint id_index;
};

/* Prototypes */

void set_buffer_address_4_patch(struct gl_strided_buffer *strided_buf);
void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf);

struct gl_strided_buffer * init_strided_buffer();
void resize_strided_buffer(int num_points, int num_comp, 
			struct gl_strided_buffer *strided_buf);
void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf);

void set_zero_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);
void set_node_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);
void select_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);


void DestroyVBO(struct VAO_ids *VAO);


#endif /* vartex_array_object_gl_h__ */
