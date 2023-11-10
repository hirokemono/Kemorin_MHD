//
//  set_transfer_matrices.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/5/23.
//

#ifndef set_transfer_matrices_h
#define set_transfer_matrices_h

#include <stdio.h>
#include <stdlib.h>

struct transfer_matrices{
    float model[16];
    float proj[16];
    float nrmat[16];
};

struct transfer_matrices * alloc_transfer_matrices(void);
struct transfer_matrices * plane_transfer_matrices(const double *orthogonal);

struct transfer_matrices * init_projection_matrix_for_map(int nx_frame, int ny_frame);

#endif /* set_transfer_matrices_h */
