/*
//  m_kemoview_mesh_buffers.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_MESH_BUFFERS_
#define M_KEMOVIEW_MESH_BUFFERS_

#include <stdio.h>
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_mesh.h"
#include "m_vertex_buffer.h"

#include "draw_patch_4_mesh_c.h"


struct MESH_buffers{
    struct gl_strided_buffer *mesh_solid_buf;
    struct gl_strided_buffer *mesh_grid_buf;
    
    struct gl_strided_buffer *mesh_node_buf;
    struct gl_index_buffer *mesh_node_index_buf;
};

/* prototypes */

struct MESH_buffers * init_MESH_buffers(void);
void dealloc_MESH_buffers(struct MESH_buffers *MESH_bufs);
void const_solid_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s,
                             struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct MESH_buffers *MESH_bufs);

#endif /* M_KEMOVIEW_MESH_BUFFERS_ */
