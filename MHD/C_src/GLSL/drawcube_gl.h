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
#include "m_vertex_buffer.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "set_cube_to_buf.h"
#include "draw_colorbar_gl.h"
#include "set_cube_VAO.h"

/* prototypes */
void set_initial_cube_VAO(struct gl_strided_buffer *cube_buf, struct gl_index_buffer *index_buf,
                          struct VAO_ids *cube_VAO);
void draw_initial_cube(struct transfer_matrices *matrices, struct VAO_ids *cube_VAO,
                       struct kemoview_shaders *kemo_shaders);

void draw_cube_edge_gl3(struct view_element *view_s, 
			struct VAO_ids *cube_VAO, struct kemoview_shaders *kemo_shaders);
void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders);

#endif
