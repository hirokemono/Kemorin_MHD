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

struct gl_index_buffer{
    int nsize_buf;
    int num_ele_buf;
    int num_each_ele;
    unsigned int *ie_buf;
};

/* prototypes */
struct gl_index_buffer * alloc_gl_index_buffer(int numele, int nnod_4_ele);
void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf,
                     struct gl_index_buffer *index_buf);


void cube_surf_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf,
                   struct gl_index_buffer *index_buf);
void cube_edge_VBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);
void cube_flat_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf);

#endif
