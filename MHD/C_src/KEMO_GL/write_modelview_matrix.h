/*
 *  write_modelview_matrix.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/11.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */


#ifndef WRITE_MODELVIEW_MATRIX_
#define WRITE_MODELVIEW_MATRIX_

#include<stdio.h>

#include "m_gl_transfer_matrix.h"
#include "m_kemoviewer_menu.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_ctl_data_4_view_transfer_c.h"

/* prototypes */

void GL_modelview_matrix_to_ctl(FILE *fp, struct view_element *view);

void copy_GL_stereo_params_to_ctl(struct view_element *view, struct streo_view_ctl_c *streo_view_c);
void copy_GL_modelview_params_to_ctl(struct view_element *view, struct modeview_ctl_c *mat_c);

void copy_GL_stereo_params_from_ctl(struct streo_view_ctl_c *streo_view_c, struct view_element *view);
void copy_GL_modelview_params_from_ctl(struct modeview_ctl_c *mat_c, struct view_element *view);

void write_GL_modelview_file(struct kv_string *filename, int iflag_view, struct view_element *view);
void read_GL_modelview_file(struct kv_string *filename, int iflag_view, struct view_element *view);

#endif
