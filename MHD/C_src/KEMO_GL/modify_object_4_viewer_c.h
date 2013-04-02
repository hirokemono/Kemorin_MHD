
/*  modify_object_4_viewer_c.h */

#ifndef MODIFY_OBJECT_4_VIEWER_C_
#define MODIFY_OBJECT_4_VIEWER_C_

#include <math.h>
#include <stdio.h>

#include "kemoviewer_param_c.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "move_draw_objects_gl.h"


/* prototypes */

void set_axis_positions(GLfloat dist, GLfloat *axis_delta, GLfloat *axis_org);
void modify_object_for_mesh(double dist, struct viewer_mesh *mesh_s, 
			struct view_element *view);
void cal_range_4_psf_grid_c(struct psf_data *psf_s, struct view_element *view);
void cal_range_4_map_grid_c(struct view_element *view);


void set_axis_positions_d(double dist, GLdouble *axis_delta, GLdouble *axis_org);

int set_viewtype(struct view_element *view, int selected, int current_view);
#endif
