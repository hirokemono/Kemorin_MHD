/*
//  m_transfer_matrices.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/5/23.
*/

#ifndef m_transfer_matrices_h_
#define m_transfer_matrices_h_

#include <stdio.h>
#include <stdlib.h>

#include "m_gl_transfer_matrix.h"
#include "invert_small_matrix_c.h"

struct transfer_matrices{
    float model[16];
    float proj[16];
    float nrmat[16];
};

struct transfer_matrices * alloc_transfer_matrices(void);
struct transfer_matrices * plane_transfer_matrices(const double *orthogonal);

struct transfer_matrices * init_projection_matrix_for_map(int nx_frame, int ny_frame);

struct transfer_matrices * transfer_matrix_to_shader(struct view_element *view_s);

void modify_mono_viewmat(struct view_element *view_s);
void modify_step_viewmat(int istep, struct view_element *view_s);
void modify_left_viewmat(struct view_element *view_s);
void modify_right_viewmat(struct view_element *view_s);

#endif /* m_transfer_matrices_ */
