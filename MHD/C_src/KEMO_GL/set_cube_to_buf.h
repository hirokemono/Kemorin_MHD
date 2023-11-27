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
#include "m_vertex_buffer.h"

struct gl_index_buffer{
    int nsize_buf;
    int num_ele_buf;
    int num_each_ele;
    unsigned int *ie_buf;
};

/* prototypes */
struct gl_index_buffer * alloc_gl_index_buffer(int numele, int nnod_4_ele);
void dealloc_gl_index_buffer(struct gl_index_buffer * alloc_gl_index_buffer);

void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf,
                     struct gl_index_buffer *index_buf);
int flatSurfCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf);


#endif
