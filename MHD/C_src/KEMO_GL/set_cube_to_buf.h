/*
 *  set_cube_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef SET_CUBE_TO_BUF_
#define SET_CUBE_TO_BUF_

#include <math.h>
#include "kemoviewer_param_c.h"
#include "vartex_array_object_gl.h"

/* prototypes */

void cube_surf_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_edge_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_flat_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

#endif
