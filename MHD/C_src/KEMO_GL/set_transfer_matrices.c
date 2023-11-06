//
//  set_transfer_matrices.c
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/5/23.
//

#include "m_gl_transfer_matrix.h"
#include "set_transfer_matrices.h"

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
    for(i=0;i<16;i++) {matrices->model[i] =  0.0;};
    for(i=0;i<9;i++)  {matrices->nrmat[i] =  0.0;};
    for(i=0;i<4;i++) {matrices->model[5*i] = 1.0;};
    for(i=0;i<3;i++) {matrices->nrmat[4*i] = 1.0;};
    
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
