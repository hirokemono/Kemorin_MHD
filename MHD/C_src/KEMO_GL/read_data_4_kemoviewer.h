
/* read_data_4_kemoviewer.h */

#ifndef READ_DATA_4_KEMOVIEWER_
#define READ_DATA_4_KEMOVIEWER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemoviewer_param_c.h"
#include "kemoviewer_base.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_mesh_menu.h"
#include "m_kemoview_psf_menu.h"
#include "m_kemoview_fline_menu.h"
#include "m_kemoview_fline.h"
#include "t_psf_edge_connect_c.h"
#include "skip_comment_c.h"
#include "icosahedron_c.h"
#include "read_viewer_mesh_c.h"
#include "read_viewer_mesh_gz_c.h"
#include "select_read_psf_viewer_c.h"
#include "take_normal_psf_c.h"
#include "check_psf_data_viewer_c.h"
#include "set_coastline_to_buf.h"
#include "set_psf_viewer.h"
#include "set_rgba_table_c.h"
#include "take_normal_surf_mesh_c.h"
#include "fline_edge_direction_c.h"
#include "set_surface_mesh_data.h"
#include "set_normal_on_node_4_mesh.h"
#include "modify_object_4_viewer_c.h"
#include "cal_viz_field_ranges.h"

/* prototypes */

void init_kemoviewer(int iflag_dmesh, struct viewer_mesh *mesh_s,
                     struct mesh_menu_val *mesh_m, struct view_element *view);

long set_psf_data_by_UCD(struct map_interpolate *map_itp,
                         struct psf_data *psf_s, struct psf_normals *psf_n,
                         struct psf_data *ucd_tmp);
void set_fline_data_by_UCD(struct psf_data *fline_d,
                           struct fline_directions *fline_dir,
                           struct psf_data *ucd_tmp);
void set_points_data_by_UCD(struct psf_data *points_d,
                            struct psf_data *ucd_tmp);

void evolution_PSF_data(struct psf_data *psf_s,
                        struct psf_normals *psf_n,
                        struct psf_data *ucd_tmp,
                        struct psf_menu_val *psf_m);
int refresh_FLINE_data(struct psf_data *ucd_tmp,
                       struct psf_data *fline_d,
                       struct fline_directions *fline_dir,
                       struct psf_menu_val *fline_m);

void set_kemoview_mesh_data(struct viewer_mesh *mesh_s,
                            struct mesh_menu_val *mesh_m, struct view_element *view);

void set_kemoview_viz_color_data(int id_color_mode,
                                 struct psf_data *viz_d,
                                 struct psf_menu_val *viz_menu);
void set_kemoview_psf_data(struct psf_data *psf_s,
                           struct psf_menu_val *psf_m);

void alloc_set_ucd_file_name_by_psf(struct psf_menu_val *psf_m, struct kv_string *ucd_m);

#endif
