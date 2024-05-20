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
	
    unsigned long npoint_draw;
};

/* Prototypes */
void Const_Simple_VAO(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_Simple_VAO(struct VAO_ids *VAO);

void Const_Simple_Index_VAO(struct VAO_ids *VAO,
                            struct gl_strided_buffer *strided_buf,
                            struct gl_index_buffer *index_buf);
void Destroy_Simple_Index_VAO(struct VAO_ids *VAO);

void Const_texture_VAO(struct gl_texure_image *kemo_texure,
                       struct gl_strided_buffer *strided_buf,
                       struct VAO_ids *VAO);
void Destroy_texture_VAO(struct VAO_ids *VAO, GLuint *textures);

void Const_Phong_VAO(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_Phong_VAO(struct VAO_ids *VAO);

void Const_Phong_Index_VAO(struct VAO_ids *VAO,
                           struct gl_strided_buffer *strided_buf,
                           struct gl_index_buffer *index_buf);
void Destroy_Phong_Index_VAO(struct VAO_ids *VAO);


void Const_VAO_Index_Phong_Texture(struct VAO_ids *VAO,
                                   struct gl_strided_buffer *strided_buf,
                                   struct gl_index_buffer *index_buf);

void Const_VAO_4_Phong_Texture(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf);
void Destroy_VAO_4_Phong_Texture(struct VAO_ids *VAO, GLuint *textures);
void Destroy_VAO(struct VAO_ids *VAO);

void Const_FBO(unsigned int width, unsigned int height, 
               struct VAO_ids *FBO);
void Destroy_FBO(struct VAO_ids *FBO);

GLuint set_texture_to_buffer(struct gl_texure_image *kemo_texure);
void DestroyVBO(struct VAO_ids *VAO);

#endif /* vartex_array_object_gl_h__ */
