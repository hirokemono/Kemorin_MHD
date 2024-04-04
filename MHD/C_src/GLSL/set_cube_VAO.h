/*
//  set_cube_VAO.h
//  
//
//  Created by Hiroaki Matsui on 11/27/23.
*/

#ifndef set_cube_VAO_h_
#define set_cube_VAO_h_

#include <stdio.h>
#include "m_vertex_buffer.h"
#include "m_vertex_buffer.h"
#include "set_cube_to_buf.h"
#include "vartex_array_object_gl.h"

/* prototypes */
void cube_surf_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf,
                   struct gl_index_buffer *index_buf);
void cube_edge_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_flat_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);


void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

#endif /* set_cube_VAO_h_ */
