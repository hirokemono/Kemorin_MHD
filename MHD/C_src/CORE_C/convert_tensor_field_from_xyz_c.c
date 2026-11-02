/***********************************************************************
!!  convert_tensor_field_from_xyz_c.c
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
 ***********************************************************************/
#include "convert_tensor_field_from_xyz_c.h"

/* symmetric tensor from cartecian coordinate to spherical coordinate */

void rr_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rr_sym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                         (float) st_xyz[i],
                                         (float) st_xyz[i+nnod],
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+3*nnod], 
                                         (float) st_xyz[i+4*nnod], 
                                         (float) st_xyz[i+5*nnod]);
    };
    return;
};
void rt_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rt_sym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod], 
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+3*nnod], 
                                         (float) st_xyz[i+4*nnod], 
                                         (float) st_xyz[i+5*nnod]);
    };
    return;
};
void rp_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rp_sym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod], 
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+3*nnod], 
                                         (float) st_xyz[i+4*nnod]);
    };
    return;
};
void tt_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tt_sym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod], 
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+3*nnod], 
                                         (float) st_xyz[i+4*nnod], 
                                         (float) st_xyz[i+5*nnod]);
    };
    return;
};
void tp_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tp_sym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod], 
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+3*nnod], 
                                         (float) st_xyz[i+4*nnod]);
    };
    return;
};
void pp_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = pp_sym_tensor_from_xyz((float) phi[i],
                                         (float) st_xyz[i],
                                         (float) st_xyz[i+nnod],
                                         (float) st_xyz[i+3*nnod]);
    };
    return;
};

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

void ss_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = ss_sym_tensor_from_xyz((float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod],
                                         (float) st_xyz[i+3*nnod]);
    };
    return;
};
void sp_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = sp_sym_tensor_from_xyz((float) phi[i],
                                         (float) st_xyz[i], 
                                         (float) st_xyz[i+nnod],
                                         (float) st_xyz[i+3*nnod]);
    };
    return;
};
void sz_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = sz_sym_tensor_from_xyz((float) phi[i],
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+4*nnod]);
    };
    return;
};
void pz_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = pz_sym_tensor_from_xyz((float) phi[i],
                                         (float) st_xyz[i+2*nnod],
                                         (float) st_xyz[i+4*nnod]);
    };
    return;
};

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

void rt_asym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rt_asym_tensor_from_xyz((float) phi[i], 
                                          (float) at_xyz[i], 
                                          (float) at_xyz[i+nnod], 
                                          (float) at_xyz[i+2*nnod]);
    };
    return;
};
void pr_asym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = pr_asym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                          (float) at_xyz[i], 
                                          (float) at_xyz[i+nnod], 
                                          (float) at_xyz[i+2*nnod]);
    };
    return;
};
void tp_asym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tp_asym_tensor_from_xyz((float) theta[i], (float) phi[i],
                                          (float) at_xyz[i], 
                                          (float) at_xyz[i+nnod], 
                                          (float) at_xyz[i+2*nnod]);
    };
    return;
};

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

void sp_asym_tensor_field_from_xyz(int nnod, 
                                   double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = (float) at_xyz[i];
    };
    return;
};
void zs_asym_tensor_field_from_xyz(int nnod, double *phi, 
                                   double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = zs_asym_tensor_from_xyz((float) phi[i], 
                                          (float) at_xyz[i], 
                                          (float) at_xyz[i+nnod], 
                                          (float) at_xyz[i+2*nnod]);
    };
    return;
};
void pz_asym_tensor_field_from_xyz(int nnod, double *phi, 
                                   double *at_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = pz_asym_tensor_from_xyz((float) phi[i], 
                                          (float) at_xyz[i], 
                                          (float) at_xyz[i+nnod], 
                                          (float) at_xyz[i+2*nnod]);
    };
    return;
};

