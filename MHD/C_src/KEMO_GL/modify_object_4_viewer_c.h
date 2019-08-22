
/*  modify_object_4_viewer_c.h */

#ifndef MODIFY_OBJECT_4_VIEWER_C_
#define MODIFY_OBJECT_4_VIEWER_C_

#include <math.h>
#include <stdio.h>

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "init_gl_lighting_c.h"


/* prototypes */

void cal_range_4_mesh_c(struct viewer_mesh *mesh_s, struct view_element *view);
void modify_object_multi_viewer_c(double dist, struct viewer_mesh *mesh_s);
void set_axis_positions(struct view_element *view, double dist, 
                        double *axis_delta, double *axis_org);
double set_tube_radius_by_view(struct view_element *view_s, double radius);

void cal_psf_viewer_range(struct psf_data **psf_s, struct kemo_array_control *psf_a,  
                          struct psf_data *fline_s, struct fline_menu_val *fline_m, 
                          struct view_element *view);

void cal_range_4_map_grid_c(struct view_element *view);


void set_viewtype(struct view_element *view, int selected);
#endif
