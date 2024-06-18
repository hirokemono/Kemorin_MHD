/*
 *  pthread_mesh_patch_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef PTHREAD_MESH_PATCH_TO_BUF_
#define PTHREAD_MESH_PATCH_TO_BUF_

#include "calypso_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "set_mesh_patch_2_gl_buf.h"


long sel_mesh_patch_to_buffer_pthread(int shading_mode, int polygon_mode,
                                      struct viewer_mesh *mesh_s,
                                      int nthreads, long ist_tri,
                                      long ntot_patch, long *iele_patch,
                                      struct gl_strided_buffer *mesh_buf);

long sel_each_mesh_grid_to_buf_pthread(long ist_patch, const int nthreads,
                                       int ist_grp, int ied_grp, int *item_grp,
                                       struct viewer_mesh *mesh_s, double f_color[4],
                                       struct gl_strided_buffer *mesh_buf);

long sel_each_grp_nod_ico_to_buf_pthread(long ist_patch, const int nthreads,
                                         int ist_grp, int ied_grp, int *item_grp,
                                         struct viewer_mesh *mesh_s,
                                         double node_diam, double f_color[4],
                                         struct gl_strided_buffer *mesh_buf,
                                         struct gl_index_buffer *index_buf);

#endif /* PTHREAD_MESH_PATCH_TO_BUF_ */
