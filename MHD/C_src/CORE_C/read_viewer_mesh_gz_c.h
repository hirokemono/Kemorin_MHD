
/* read_viewer_mesh_gz_c.h */

#ifndef READ_VIEWER_MESH_GZ_C_
#define READ_VIEWER_MESH_GZ_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "calypso_zlib_io_c.h"

/* Prototypes */ 

int read_viewer_mesh_gz_c(const char *filename, struct viewer_mesh *mesh_s);

#endif
