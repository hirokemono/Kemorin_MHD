/*
 *  drawcube_gl_old.h
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
#include "m_vertex_buffer.h"

/* prototypes */

void drawCube (GLfloat fSize);
void drawCube_array (GLfloat fSize);
void drawCube_Element(GLfloat fSize);
void drawCube_flat(GLfloat fSize, 
				   struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO);
void drawCube_Element2(GLfloat fSize, 
					   struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO);

#endif
