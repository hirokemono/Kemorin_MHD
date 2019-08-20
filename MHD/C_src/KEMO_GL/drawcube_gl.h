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
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_cube_to_buf.h"

/* prototypes */

void set_initial_cube_VAO(struct view_element *view_s, struct VAO_ids *cube_VAO);
void draw_initial_cube(struct view_element *view_s, struct VAO_ids *cube_VAO, 
			struct kemoview_shaders *kemo_shaders);

void draw_cube_edge_gl3(struct view_element *view_s, 
			struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders);
void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders);

#endif
