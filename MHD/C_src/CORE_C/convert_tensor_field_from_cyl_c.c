/***********************************************************************
 !!  convert_tensor_field_from_cyl_c.c
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 !!   uxux = (as)^2 *(x*x *usus - x*y *usup
 !!                 - x*y *usup + y*y *upup)
 !!   uxuy = (as)^2 *(x*y *usus + x*x *usup
 !!                 - y*y *usup - y*x *upup)
 !!   uxuz = (as)^2 *(x*s *usuz - y*s *upuz)
 !!   uyuy = (as)^2 *(y*y *usus + x*y *usup
 !!                 + x*y *usup + x*x *upup)
 !!   uyuz = (as)^2 *(y*s *usuz + x*s *upuz)
 !!   uzuz = (as)^2 * s*s* uzuz
 !!
 !!
 !!   urut =         uzus
 !!   upur = (as) * (-s* usup + z* upuz)
 !!   utup = (as) * ( z* usup + s* upuz)
 ***********************************************************************/
#include "convert_tensor_field_from_cyl_c.h"

/* prototypes */

/* symmetric tensor from cylindrical coordinate to cartesian coordinate */
void xx_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = xx_sym_tensor_from_cyl((float) phi[i],
                                         (float) st_cyl[i],
                                         (float) st_cyl[i+nnod],
                                         (float) st_cyl[i+3*nnod]);
    };
    return;
};

void xy_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = xy_sym_tensor_from_cyl((float) phi[i],
                                         (float) st_cyl[i], 
                                         (float) st_cyl[i+nnod],
                                         (float) st_cyl[i+3*nnod]);
    };
    return;
};

void xz_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = xz_sym_tensor_from_cyl((float) phi[i],
                                         (float) st_cyl[i+2*nnod],
                                         (float) st_cyl[i+4*nnod]);
    };
    return;
};

void yy_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = yy_sym_tensor_from_cyl((float) phi[i],
                                         (float) st_cyl[i], 
                                         (float) st_cyl[i+nnod],
                                         (float) st_cyl[i+3*nnod]);
   };
    return;
};

void yz_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = yz_sym_tensor_from_cyl((float) phi[i],
                                         (float) st_cyl[i+2*nnod],
                                         (float) st_cyl[i+4*nnod]);
    };
    return;
};

/* symmetric tensor from cylindrical coordinate to spherical coordinate */

void rr_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rr_sym_tensor_from_cyl((float) theta[i],
                                         (float) st_cyl[i],
                                         (float) st_cyl[i+2*nnod],
                                         (float) st_cyl[i+5*nnod]);
    };
    return;
};

void rt_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rt_sym_tensor_from_cyl((float) theta[i],
                                         (float) st_cyl[i],
                                         (float) st_cyl[i+2*nnod],
                                         (float) st_cyl[i+5*nnod]);
    };
    return;
};

void rp_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = rp_sym_tensor_from_cyl((float) theta[i],
                                         (float) st_cyl[i+nnod],
                                         (float) st_cyl[i+4*nnod]);
    };
    return;
};

void tt_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tt_sym_tensor_from_cyl((float) theta[i],
                                         (float) st_cyl[i],
                                         (float) st_cyl[i+2*nnod],
                                         (float) st_cyl[i+5*nnod]);
    };
    return;
};

void tp_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tp_sym_tensor_from_cyl((float) theta[i],
                                         (float) st_cyl[i+nnod],
                                         (float) st_cyl[i+4*nnod]);
    };
    return;
};

/* antisymmetric tensor from cylindrical coordinate to spherical coordinate */

void rt_asym_tensor_field_from_cyl(int nnod, double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = (float) at_cyl[i+nnod];
    };
    return;
};

void pr_asym_tensor_field_from_cyl(int nnod, double *theta, 
                                   double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = pr_asym_tensor_from_cyl((float) theta[i],
                                          (float) at_cyl[i], 
                                          (float) at_cyl[i+2*nnod]);
    };
    return;
};

void tp_asym_tensor_field_from_cyl(int nnod, double *theta, 
                                   double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = tp_asym_tensor_from_cyl((float) theta[i], 
                                          (float) at_cyl[i], 
                                          (float) at_cyl[i+2*nnod]);
    };
    return;
};

/* antisymmetric tensor from cylindrical coordinate to cartesian coordinate */

void xy_asym_tensor_field_from_cyl(int nnod, double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = (float) at_cyl[i];
    };
    return;
};

void zx_asym_tensor_field_from_cyl(int nnod, double *phi, 
                                   double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = zx_asym_tensor_from_cyl((float) phi[i], 
                                          (float) at_cyl[i+nnod],
                                          (float) at_cyl[i+2*nnod]);
    };
    return;
};

void yz_asym_tensor_field_from_cyl(int nnod, double *phi, 
                                   double *at_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = yz_asym_tensor_from_cyl((float) phi[i], 
                                          (float) at_cyl[i+nnod],
                                          (float) at_cyl[i+2*nnod]);
    };
    return;
};
