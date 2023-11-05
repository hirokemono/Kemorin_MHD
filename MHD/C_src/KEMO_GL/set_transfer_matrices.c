//
//  set_transfer_matrices.c
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/5/23.
//

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
