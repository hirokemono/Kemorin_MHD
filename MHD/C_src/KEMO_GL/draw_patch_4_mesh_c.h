
/* draw_patch_4_mesh_c.h */

#ifndef DRAW_PATCH_4_MESH_C_
#define DRAW_PATCH_4_MESH_C_

#include <stdlib.h>
#include "kemoviewer_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "sort_by_patch_distance.h"
#include "rainbow_color_code_c.h"
#include "set_mesh_patch_2_gl_buf.h"
#include "set_mesh_grid_2_gl_buf.h"
#include "set_mesh_node_2_gl_buf.h"
#include "pthread_mesh_patch_to_buf.h"
#include "bitonic_sort_float_pthread.h"
#include "const_mesh_patch_table_for_gl.h"

struct mesh_sorting_work{
    long nextP2_trans_patch;
    long ntotP2_trans_patch;

    long *index_trans_patch;
    float *z_trans_patch;
    
    double *z_ele_view;
};


/* prototypes */
void const_solid_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s, struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct gl_strided_buffer *mesh_solid_buf,
                             struct gl_strided_buffer *mesh_grid_buf,
                             struct gl_strided_buffer *mesh_node_buf);
void const_trans_mesh_buffer(int nthreads,
                             struct viewer_mesh *mesh_s,
                             struct mesh_menu_val *mesh_m,
                             struct view_element *view_s,
                             struct gl_strided_buffer *mesh_trns_buf);
#endif
