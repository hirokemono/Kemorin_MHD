/*
 *  set_normal_on_node_4_mesh.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/21.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef SET_NORMAL_ON_NODE_4_MESH_
#define SET_NORMAL_ON_NODE_4_MESH_

#include <stdlib.h>
#include <math.h>

#include "m_surface_mesh_4_viewer_c.h"
#include "set_surface_mesh_data.h"

/* prototypes */

void set_normal_on_node_4_mesh(struct viewer_mesh *mesh_s);
void set_mesh_patch_group_id(struct viewer_mesh *mesh_s);

#endif
