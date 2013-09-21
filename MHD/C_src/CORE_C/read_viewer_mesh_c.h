
/* read_viewer_mesh_c.h */

#ifndef READ_VIEWER_MESH_C_
#define READ_VIEWER_MESH_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "m_surface_mesh_4_viewer_c.h"
#include "skip_comment_c.h"

/* Prototypes */ 

int read_viewer_mesh(const char *file_name, struct viewer_mesh *mesh_s);

#endif
