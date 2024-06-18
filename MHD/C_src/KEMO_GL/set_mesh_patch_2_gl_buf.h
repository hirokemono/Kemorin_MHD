/*
 *  set_mesh_patch_2_gl_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef SET_MESH_PATCH_2_GL_BUF_
#define SET_MESH_PATCH_2_GL_BUF_

#include "calypso_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "set_primitives_to_gl_buffer.h"

long add_mesh_node_ico_to_buf(long ist_patch, int *istack_grp, int num_pe_sf,
                              int *iflag_domain, long *istack_patch_pe);
long set_each_group_node_ico_to_buf(const long ist_tri, 
                                    long ist_grp, long ied_grp, int *item_grp,
                                    struct viewer_mesh *mesh_s, double node_diam,
                                    double f_color[4], 
                                    struct gl_strided_buffer *mesh_buf,
                                    struct gl_index_buffer *index_buf);

long count_mesh_edge_buf(long ist_edge, int *iflag_domain, int *istack_grp,
                         struct viewer_mesh *mesh_s, long *istack_edge_pe);
long set_each_mesh_grid_to_buf(int ist, int ied, int *item_grp,
                               struct viewer_mesh *mesh_s,
                               double f_color[4], long ist_edge,
                               struct gl_strided_buffer *strided_buf);

long set_mesh_patch_to_buffer(int shading_mode, int polygon_mode,
                              struct viewer_mesh *mesh_s, long ist_tri,
                              long ist_ele, long ied_ele, long *iele_patch,
                              struct gl_strided_buffer *mesh_buf);

#endif /* SET_MESH_PATCH_2_GL_BUF_*/
