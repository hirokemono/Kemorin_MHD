/*
 *  set_surface_mesh_data.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/19.
 *
 */

#ifndef SET_SURFACE_MESH_DATA_
#define SET_SURFACE_MESH_DATA_

#include <math.h>
#include "m_surface_mesh_4_viewer_c.h"
#include "check_viewer_mesh_c.h"

/* prototypes */

void set_surface_mesh_size(struct viewer_mesh *mesh_s);
void set_surface_normal_4_each_node(struct viewer_mesh *mesh_s);

#endif
