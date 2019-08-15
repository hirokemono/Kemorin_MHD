/*
 *  drawcube_gl.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef DRRAWCUBE_GL_
#define DRRAWCUBE_GL_

#include <math.h>
#include "kemoviewer_param_c.h"
#include "vartex_array_object_gl.h"

/* prototypes */

void drawCube (GLfloat fSize);
void drawCube_array (GLfloat fSize);

void drawCube_Element2(GLfloat fSize, 
				   struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO);
void drawCube_flat(GLfloat fSize, 
				   struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO);

void cube_surf_VBO(GLfloat fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_edge_VBO(GLfloat fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_flat_VBO(GLfloat fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

#endif
