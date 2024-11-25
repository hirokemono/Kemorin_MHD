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

struct initial_cube_buffers{
    struct gl_strided_buffer *cube_buf;
    struct gl_index_buffer *cube_index_buf;
};


/* prototypes */
struct initial_cube_buffers * init_initial_cube_buffers(void);
void dealloc_initial_cube_buffers(struct initial_cube_buffers *initial_bufs);

void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf,
                     struct gl_index_buffer *index_buf);
int flatSurfCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf);

long flatEdgeCube_VBO(long icou, float fSize,
                      struct gl_strided_buffer *strided_buf,
                      struct gl_local_buffer_address *point_buf);
long flatNodeCube_VBO(long icou, float fSize,
                      struct gl_strided_buffer *strided_buf,
                      struct gl_local_buffer_address *point_buf);

#endif
