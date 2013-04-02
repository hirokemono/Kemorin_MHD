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

/* prototypes */

void output_GL_modelview_parameters(FILE *fp, struct view_element *view);
void output_GL_modelview_matrix(FILE *fp, struct view_element *view);
void output_GL_projection_matrix(FILE *fp, struct view_element *view);
void output_stereo_parameter(FILE *fp, struct view_element *view);

void input_GL_modelview_parameters(FILE *fp, struct view_element *view);
void input_GL_modelview_matrix(FILE *fp, struct view_element *view);
void input_GL_projection_matrix(FILE *fp, struct view_element *view);
void input_stereo_parameter(FILE *fp, struct view_element *view);

void write_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view);
void read_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view);

#endif
