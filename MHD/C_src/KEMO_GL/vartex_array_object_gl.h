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

struct gl_strided_buffer{
	int nsize_buf;
	int istride;
	
	int ncomp_buf;
	int num_nod_buf;
	
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
	
	GLuint npoint_draw;
};

/* Prototypes */

void set_buffer_address_4_patch(int num_points, struct gl_strided_buffer *strided_buf);
void set_buffer_address_4_map(struct gl_strided_buffer *strided_buf);
void alloc_strided_buffer(int num_points, int num_comp, 
			struct gl_strided_buffer *strided_buf);

struct gl_strided_buffer * init_strided_buffer(int num_points);
void resize_strided_buffer(int num_points, int num_comp, 
			struct gl_strided_buffer *strided_buf);
void dealloc_strided_buffer(struct gl_strided_buffer *strided_buf);

void set_zero_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);
void set_node_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);
void select_stride_VBO(int inum, struct gl_strided_buffer *strided_buf);

void Const_VAO_4_Simple(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Const_VAO_4_Texture(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Const_VAO_4_Phong(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Const_VAO_4_Phong_Texture(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);

void Destroy_VAO(struct VAO_ids *VAO);

void Destroy_Simple_VAO(struct VAO_ids *VAO);
void Destroy_Texture_VAO(struct VAO_ids *VAO, GLuint *textures);
void Destroy_Phong_VAO(struct VAO_ids *VAO);
void Destroy_Phong_Texture_VAO(struct VAO_ids *VAO, GLuint *textures);

void DestroyVBO(struct VAO_ids *VAO);

GLuint set_texture_to_buffer(const int iwidth, const int iheight, 
			const unsigned char *rgba);

#endif /* vartex_array_object_gl_h__ */
