/*
//  vartex_array_object_gl.c
//  
//
//  Created by Hiroaki Matsui on 2019/08/05.
*/

#include "vartex_array_object_gl.h"


void Const_VAO_4_Simple(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf){
    VAO->npoint_draw = strided_buf->num_nod_buf;
    if(VAO->npoint_draw <= 0) return;
    
	glBindVertexArray(VAO->id_VAO);
	glDeleteBuffers(1, &VAO->id_vertex);
	
	glGenBuffers(1, &VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glBindVertexArray(0);
};

void Destroy_VAO_4_Simple(struct VAO_ids *VAO)
{
    GLenum ErrorCheckValue = glGetError();
    
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(0);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    glDeleteBuffers(1, &VAO->id_color);
    glDeleteBuffers(1, &VAO->id_vertex);
    
    glBindVertexArray(0);
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


void Const_VAO_4_Texture(struct VAO_ids *VAO, const struct gl_strided_buffer *strided_buf){
	glDeleteBuffers(1, &VAO->id_vertex);
	
	glGenBuffers(1, &VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_tex * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
};

void Destroy_VAO_4_Texture(struct VAO_ids *VAO, GLuint *textures)
{
    GLenum ErrorCheckValue = glGetError();
    
    glBindVertexArray(VAO->id_VAO);
    
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(0);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    glDeleteTextures(1, textures);
    
    glDeleteBuffers(1, &VAO->id_color);
    glDeleteBuffers(1, &VAO->id_vertex);
    glBindVertexArray(0);
}


void Const_VAO_4_Phong(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf){
    VAO->npoint_draw = strided_buf->num_nod_buf;
    if(VAO->npoint_draw <= 0) return;
    
	glBindVertexArray(VAO->id_VAO);
	glDeleteBuffers(1, &VAO->id_vertex);

	glGenBuffers(1, &VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glBindVertexArray(0);
	return;
};

void Destroy_VAO_4_Phong(struct VAO_ids *VAO)
{
    GLenum ErrorCheckValue = glGetError();
    
    glBindVertexArray(VAO->id_VAO);
    
    glDisableVertexAttribArray(3);
    glDisableVertexAttribArray(2);
    glDisableVertexAttribArray(0);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    glDeleteBuffers(1, &VAO->id_color);
    glDeleteBuffers(1, &VAO->id_vertex);
    glBindVertexArray(0);
    
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


void Const_VAO_4_Phong_Texture(struct VAO_ids *VAO, struct gl_strided_buffer *strided_buf){
    VAO->npoint_draw = strided_buf->num_nod_buf;
    if(VAO->npoint_draw <= 0) return;
    
	glBindVertexArray(VAO->id_VAO);
	glDeleteBuffers(1, &VAO->id_vertex);
	
	glGenBuffers(1, &VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * strided_buf->num_nod_buf*strided_buf->ncomp_buf,
				 strided_buf->v_buf, GL_STATIC_DRAW);

	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_tex * sizeof(GL_FLOAT)));

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	glBindVertexArray(0);
	return;
};

void Destroy_VAO_4_Phong_Texture(struct VAO_ids *VAO, GLuint *textures)
{
    GLenum ErrorCheckValue = glGetError();
    
    glBindVertexArray(VAO->id_VAO);
    
    glDisableVertexAttribArray(4);
    glDisableVertexAttribArray(3);
    glDisableVertexAttribArray(2);
    glDisableVertexAttribArray(0);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    glDeleteTextures(1, textures);
    
    glDeleteBuffers(1, &VAO->id_color);
    glDeleteBuffers(1, &VAO->id_vertex);
    glBindVertexArray(0);
    
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

void Destroy_VAO(struct VAO_ids *VAO){
    glBindVertexArray(0);
    glDeleteVertexArrays(1, &(VAO->id_VAO));
};



void Const_FBO(unsigned int width, unsigned int height, 
               struct VAO_ids *FBO){
    //Somewhere at initialization
    glGenFramebuffers(1,&(FBO->id_VAO));
    glGenRenderbuffers(1,&(FBO->id_texure));
    glBindRenderbuffer(GL_RENDERBUFFER, FBO->id_texure);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_RGB8, width, height);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FBO->id_VAO);
    glFramebufferRenderbuffer(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                              GL_RENDERBUFFER, FBO->id_texure);
    return;
}

void Destroy_FBO(struct VAO_ids *FBO){
    glDeleteFramebuffers(1,&(FBO->id_VAO));
    glDeleteRenderbuffers(1,&(FBO->id_texure));
    return;
}


GLuint set_texture_to_buffer(struct gl_texure_image *kemo_texure){
	/* Preference for resiging texture */
    GLuint textureName;
	glGenTextures(1, &textureName);
	glBindTexture(GL_TEXTURE_2D , textureName);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
                 kemo_texure->nipxel_xy[0],
                 kemo_texure->nipxel_xy[1],
				 0, GL_RGBA, GL_UNSIGNED_BYTE,
                 kemo_texure->texure_rgba);
	return textureName;
};

void const_texture_VBO(struct gl_texure_image *kemo_texure,
                       struct gl_strided_buffer *strided_buf,
                       struct VAO_ids *VAO){
    VAO->npoint_draw = strided_buf->num_nod_buf;
    if(VAO->npoint_draw <= 0) return;

    glBindVertexArray(VAO->id_VAO);
    Const_VAO_4_Texture(VAO, strided_buf);
    VAO->id_texure = set_texture_to_buffer(kemo_texure);
    glBindVertexArray(0);
    return;
};

void DestroyVBO(struct VAO_ids *VAO)
{
    GLenum ErrorCheckValue = glGetError();
    
    glBindVertexArray(VAO->id_VAO);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(0);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    glDeleteBuffers(1, &VAO->id_color);
    glDeleteBuffers(1, &VAO->id_vertex);
    
    glBindVertexArray(0);
    glDeleteVertexArrays(1, &(VAO->id_VAO));
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

