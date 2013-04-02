
/* read_viewer_mesh_c.h */

#ifndef READ_VIEWER_MESH_C_
#define READ_VIEWER_MESH_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "m_surface_mesh_4_viewer_c.h"
#include "skip_comment_c.h"
#include "read_viewer_mesh_gz_c.h"
#include "take_normal_surf_mesh_c.h"
#include "set_surface_mesh_data.h"
#include "set_normal_on_node_4_mesh.h"

/* Prototypes */ 

void read_viewer_mesh(const char *file_head, struct viewer_mesh *mesh_s);
void check_gzip_viewer_mesh_first(const char *file_head, struct viewer_mesh *mesh_s);

void set_viewer_mesh(struct viewer_mesh *mesh_s);

#endif
