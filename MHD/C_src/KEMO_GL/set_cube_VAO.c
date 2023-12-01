/*
//  set_cube_VAO.c
//  
//
//  Created by Hiroaki Matsui on 11/27/23.
*/

#include "set_cube_VAO.h"

static GLuint cube_edge [12][2] = {
            {0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6},
            {6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};


/* draw simple cube based on current modelview and projection matrices */

void cube_surf_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf,
                   struct gl_index_buffer *index_buf)
{
    GLenum ErrorCheckValue = glGetError();
    
    Const_VAO_4_Phong(VAO_quad, gl_buf);
    
    glBindVertexArray(VAO_quad->id_VAO);
    glDeleteBuffers(1, &VAO_quad->id_index);
    /* Create index buffer on GPU, and then copy from CPU */
    glGenBuffers(1, &VAO_quad->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, (36 * sizeof(unsigned int)),
                 index_buf->ie_buf, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    
    /*
    ErrorCheckValue = glGetError();
    if (ErrorCheckValue != GL_NO_ERROR)
    {
        fprintf(
                stderr,
                "ERROR: Could not create a VBO: %s \n",
                gluErrorString(ErrorCheckValue)
                );
        
        exit(-1);
    }
    */
}


void cube_edge_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
    GLenum ErrorCheckValue = glGetError();
    
    glDeleteBuffers(1, &VAO_quad->id_vertex);
    
    glGenBuffers(1, &VAO_quad->id_vertex);
    glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
                 gl_buf->v_buf, GL_STATIC_DRAW);
    
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_norm * sizeof(GL_FLOAT)));
    
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    
    glDeleteBuffers(1, &VAO_quad->id_index);
    
    /* Create index buffer on GPU, and then copy from CPU */
    glGenBuffers(1, &VAO_quad->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*24, cube_edge, GL_STATIC_DRAW);
    
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    /*
    ErrorCheckValue = glGetError();
    if (ErrorCheckValue != GL_NO_ERROR)
    {
        fprintf(
                stderr,
                "ERROR: Could not create a VBO: %s \n",
                gluErrorString(ErrorCheckValue)
                );
        
        exit(-1);
    }
    */
}

void cube_flat_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
    int icou = 0;
    icou = flatSurfCube_VBO(icou, fSize, gl_buf);
    
    GLenum ErrorCheckValue = glGetError();
    
    glGenVertexArrays(1, &(VAO_quad->id_VAO));
    glBindVertexArray(VAO_quad->id_VAO);
    
    glDeleteBuffers(1, &VAO_quad->id_vertex);

    glGenBuffers(1, &VAO_quad->id_vertex);
    glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
                 gl_buf->v_buf, GL_STATIC_DRAW);
    
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_norm * sizeof(GL_FLOAT)));
    
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    /*
    ErrorCheckValue = glGetError();
    if (ErrorCheckValue != GL_NO_ERROR)
    {
        fprintf(
                stderr,
                "ERROR: Could not create a VBO: %s \n",
                gluErrorString(ErrorCheckValue)
                );
        
        exit(-1);
    }
    */
    glBindVertexArray(0);
}


void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
    float Vertices[] = {
        -0.8f,  0.8f, 0.0f,
        0.8f,  0.8f, 0.0f,
        -0.8f, -0.8f, 0.0f,
        0.8f, -0.8f, 0.0f
    };
    
    float Colors[] = {
        1.0f, 0.0f, 0.0f, 1.0f,
        0.0f, 1.0f, 0.0f, 1.0f,
        0.0f, 0.0f, 1.0f, 1.0f,
        1.0f, 1.0f, 1.0f, 1.0f
    };
    
    GLuint quad_tri_faces [2][3] = {{0, 2, 1},  {2, 3, 1}};
    
    int i, nd;
    
    for(i=0;i<gl_buf->num_nod_buf;i++){
        for(nd=0;nd<3;nd++){
            gl_buf->v_buf[nd + gl_buf->ist_xyz + i*gl_buf->ncomp_buf] = Vertices[nd+3*i];
        };
        for(nd=0;nd<4;nd++){
            gl_buf->v_buf[nd + gl_buf->ist_csurf + i*gl_buf->ncomp_buf] = Colors[nd+4*i];
        };
    };
    
    GLenum ErrorCheckValue = glGetError();
    
    glDeleteBuffers(1, &VAO_quad->id_vertex);

    glGenBuffers(1, &VAO_quad->id_vertex);
    glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
    glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
                 gl_buf->v_buf, GL_STATIC_DRAW);
    
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, gl_buf->istride,
                          (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
    
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    
    glDeleteBuffers(1, &VAO_quad->id_index);
    /* Create index buffer on GPU, and then copy from CPU */
    glGenBuffers(1, &VAO_quad->id_index);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(GLuint)*6, quad_tri_faces, GL_STATIC_DRAW);
    
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    
    ErrorCheckValue = glGetError();
    /*
    if (ErrorCheckValue != GL_NO_ERROR)
    {
        fprintf(
                stderr,
                "ERROR: Could not create a VBO: %s \n",
                gluErrorString(ErrorCheckValue)
                );
        
        exit(-1);
    }
     */
}
