/*
//  m_transfer_matrices.c
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/5/23.
*/

#include "m_transfer_matrices.h"

struct transfer_matrices * alloc_transfer_matrices(void){
    struct transfer_matrices *matrices;
    if((matrices = (struct transfer_matrices *) malloc(sizeof(struct transfer_matrices))) == NULL){
        printf("malloc error for transfer_matrices\n");
        exit(0);
    };
    return matrices;
};

struct transfer_matrices * plane_transfer_matrices(const double *orthogonal)
{
    int i;
    struct transfer_matrices *matrices = alloc_transfer_matrices();
    for(i=0;i<16;i++)  {matrices->model[i] =  0.0;};
    for(i=0;i<16;i++)  {matrices->nrmat[i] =  0.0;};
    for(i=0;i<4;i++)   {matrices->model[5*i] = 1.0;};
    for(i=0;i<4;i++)   {matrices->nrmat[5*i] = 1.0;};
    
    for(i=0;i<16;i++) {matrices->proj[i] = (float) orthogonal[i];};
    return matrices;
};

struct transfer_matrices * init_projection_matrix_for_map(int nx_frame, int ny_frame){
    double xwin, ywin;
    if(ny_frame > nx_frame) {
        xwin = 2.05;
        ywin = 2.05 * (double)ny_frame / (double)nx_frame;
    } else{
        xwin = 1.7 * (double)nx_frame / (double)ny_frame;
        ywin = 1.7;
    }
    double *orthogonal = orthogonal_projection_mat_c(-xwin, xwin, -ywin, ywin,
                                                     -1.0, 1.0);
    struct transfer_matrices *matrices = plane_transfer_matrices(orthogonal);
    free(orthogonal);
    return matrices;
}

struct transfer_matrices * transfer_matrix_to_shader(struct view_element *view_s){
    double a_inv[16];
    int i;
    
    struct transfer_matrices *matrices = alloc_transfer_matrices();
    cal_inverse_44_matrix_c(view_s->mat_object_2_eye, a_inv);
    for(i=0;i<16;i++) {matrices->model[i] = (float) view_s->mat_object_2_eye[i];};
    for(i=0;i<16;i++) {matrices->proj[i] = (float) view_s->mat_eye_2_clip[i];};
    
    for(i=0;i<3;i++) {
        matrices->nrmat[4*i  ] = (float) a_inv[  i];
        matrices->nrmat[4*i+1] = (float) a_inv[4+i];
        matrices->nrmat[4*i+2] = (float) a_inv[8+i];
        matrices->nrmat[4*i+3] =  0.0;

        matrices->nrmat[4*3+i] =  0.0;
    };
    matrices->nrmat[4*3+3] = (float) 1.0;
    return matrices;
};

void modify_mono_viewmat(struct view_element *view_s)
{
    update_projection_struct(view_s);
    modify_view_by_struct(view_s);
    return;
};

void modify_step_viewmat(int istep, struct view_element *view_s)
{
    update_step_projection_struct(istep, view_s);
    modify_step_view_by_struct(istep, view_s);
    return;
};

void modify_left_viewmat(struct view_element *view_s)
{
    update_left_projection_struct(view_s);
    modify_left_view_by_struct(view_s);
    return;
};

void modify_right_viewmat(struct view_element *view_s)
{
    update_right_projection_struct(view_s);
    modify_right_view_by_struct(view_s);
    return;
};

