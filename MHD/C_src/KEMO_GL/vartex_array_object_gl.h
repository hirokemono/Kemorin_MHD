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
#include "kemoviewer_gl.h"
#include "m_vertex_buffer.h"

struct VAO_ids{
    unsigned int id_VAO;
	
    unsigned int id_vertex;
    unsigned int id_color;
    
    unsigned int id_texure;

    unsigned int id_index;
	
    unsigned int npoint_draw;
};

/* Prototypes */
void Const_VAO_4_Simple(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_VAO_4_Simple(struct VAO_ids *VAO);

void Const_VAO_4_Texture(struct VAO_ids *VAO, const struct gl_strided_buffer *strided_buf);
void Destroy_VAO_4_Texture(struct VAO_ids *VAO, GLuint *textures);

void Const_VAO_4_Phong(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_VAO_4_Phong(struct VAO_ids *VAO);

void Const_VAO_4_Phong_Texture(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_VAO_4_Phong_Texture(struct VAO_ids *VAO, GLuint *textures);
void Destroy_VAO(struct VAO_ids *VAO);

void Const_FBO(unsigned int width, unsigned int height, 
               struct VAO_ids *FBO);
void Destroy_FBO(struct VAO_ids *FBO);

GLuint set_texture_to_buffer(const int iwidth, const int iheight, 
                             const unsigned char *rgba);
void const_texture_VBO(const int iwidth, const int iheight, const unsigned char *rgba,
                       struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void DestroyVBO(struct VAO_ids *VAO);

#endif /* vartex_array_object_gl_h__ */
