
/* read_data_4_kemoviewer.h */

#ifndef READ_DATA_4_KEMOVIEWER_
#define READ_DATA_4_KEMOVIEWER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemoviewer_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoviewer_menu.h"
#include "skip_comment_c.h"
#include "icosahedron_c.h"
#include "read_viewer_mesh_c.h"
#include "read_psf_data_viewer_c.h"
#include "move_draw_objects_gl.h"
#include "take_normal_psf_c.h"
#include "check_psf_data_viewer_c.h"
#include "init_gl_lighting_c.h"
#include "modify_object_4_viewer_c.h"
#include "draw_mapgrid.h"
#include "read_psf_data_viewer_c.h"
#include "set_psf_viewer.h"
#include "set_rgba_table_c.h"

/* prototypes */

void init_kemoviewer(int iflag_dmesh, struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m, struct view_element *view);

int evolution_PSF_data(struct psf_data *psf_s, struct psf_data *ucd_tmp, struct psf_menu_val *psf_m);
int refresh_FLINE_data(const char *file_head, int istep,
					   struct psf_data *fline_s, struct psf_data *ucd_tmp);

void set_kemoview_mesh_data(struct viewer_mesh *mesh_s,
			struct mesh_menu_val *mesh_m, struct view_element *view);

void set_kemoview_psf_data(struct psf_data *psf_s, struct psf_data *ucd_tmp,
			struct mesh_menu_val *mesh_m, struct psf_menu_val *psf_m,
			struct view_element *view);

void set_kemoview_fline_data(struct psf_data *fline_s, struct psf_data *ucd_tmp, 
							struct mesh_menu_val *mesh_m, struct fline_menu_val *fline_m,
							 struct view_element *view, int num_loaded);

#endif
