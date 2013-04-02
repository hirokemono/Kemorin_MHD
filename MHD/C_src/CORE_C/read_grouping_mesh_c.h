
/* read_grouping_mesh_c.h */


#ifndef READ_GROUPING_MESH_
#define READ_GROUPING_MESH_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "m_grouping_mesh_4_viewer_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "skip_comment_c.h"


/* Prototypes */ 

void read_grouping_mesh_c(char *file_name, struct grouping_data *mesh_g, struct viewer_mesh *mesh_s);
void read_coef_file_for_snap_c(char *file_name, struct grouping_data *mesh_g, int istep_target);
void read_tave_coef_file_c(char *file_name, struct grouping_data *mesh_g);


#endif
