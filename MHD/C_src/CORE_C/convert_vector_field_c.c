/***********************************************************************
!!  convert_vector_field_c.c
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
 ***********************************************************************/
#include "convert_vector_field_c.h"

/* vector field to cartecian coordinate */

void xyz_vector_field_by_sph_c(int nnod, const double *theta, const double *phi,
                               double *v_sph, float *v_xyz){
    int i;
    for(i=0;i<nnod;i++){
        v_xyz[i] =        x_vector_from_sph((float) theta[i], (float) phi[i], 
                                            (float) v_sph[i], 
                                            (float) v_sph[i+nnod], 
                                            (float) v_sph[i+2*nnod]);
        v_xyz[i+nnod] =   y_vector_from_sph((float) theta[i], (float) phi[i], 
                                            (float) v_sph[i], 
                                            (float) v_sph[i+nnod], 
                                            (float) v_sph[i+2*nnod]);
        v_xyz[i+2*nnod] = z_vector_from_sph((float) theta[i],
                                            (float) v_sph[i], 
                                            (float) v_sph[i+nnod]);
    };
    return;
};

void xyz_vector_field_by_cyl_c(int nnod, const double *phi, 
                               double *v_cyl, float *v_xyz){
    int i;
    for(i=0;i<nnod;i++){
        v_xyz[i] =        x_vector_from_cyl((float) phi[i], 
                                            (float) v_cyl[i], 
                                            (float) v_cyl[i+nnod]);
        v_xyz[i+nnod] =   y_vector_from_cyl((float) phi[i], 
                                            (float) v_cyl[i], 
                                            (float) v_cyl[i+nnod]);
        v_xyz[i+2*nnod] = (float) v_cyl[i+2*nnod];
    };
    return;
};

/* vector components from cartecian coordinate to spherical coordinate */

void r_vector_field_from_xyz(int nnod, double *theta, double *phi,
                             double *v_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = r_vector_from_xyz(theta[i], phi[i],
                                    v_xyz[i],
                                    v_xyz[i+nnod],
                                    v_xyz[i+2*nnod]);
    };
    return;
};

void theta_vector_field_from_xyz(int nnod, double *theta, double *phi,
                                 double *v_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = theta_vector_from_xyz(theta[i], phi[i],
                                        v_xyz[i],
                                        v_xyz[i+nnod],
                                        v_xyz[i+2*nnod]);
    };
    return;
};

void phi_vector_field_from_xyz(int nnod, double *phi,
                                  double *v_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = phi_vector_from_xyz(phi[i],
                                      v_xyz[i],
                                      v_xyz[i+nnod]);
    };
    return;
};

/*  vector components from cartecian coordinate to cylindrical coordinate */

void s_vector_field_from_xyz(int nnod, double *phi,
                             double *v_xyz, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = s_vector_from_xyz((float) phi[i],
                                    (float) v_xyz[i], 
                                    (float) v_xyz[i+nnod]);
    };
    return;
};

/* vector components from spherical coordinate to cartecian coordinate */

void x_vector_field_from_sph(int nnod, double *theta, double *phi,
                             double *v_sph, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = x_vector_from_sph((float) theta[i], (float) phi[i], 
                                    (float) v_sph[i], 
                                    (float) v_sph[i+nnod], 
                                    (float) v_sph[i+2*nnod]);
    };
    return;
};

void y_vector_field_from_sph(int nnod, double *theta, double *phi,
                             double *v_sph, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = y_vector_from_sph((float) theta[i], (float) phi[i], 
                                    (float) v_sph[i], 
                                    (float) v_sph[i+nnod], 
                                    (float) v_sph[i+2*nnod]);
    };
    return;
};

void z_vector_field_from_sph(int nnod, double *theta,
                             double *v_sph, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = z_vector_from_sph((float) theta[i],
                                    (float) v_sph[i], 
                                    (float) v_sph[i+nnod]);
    };
    return;
};

/* vector components from cylindrical coordinate to cartecian coordinate */

void x_vector_field_from_cyl(int nnod, double *phi,
                             double *v_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = x_vector_from_cyl((float) phi[i], 
                                    (float) v_cyl[i], 
                                    (float) v_cyl[i+nnod]);
    };
    return;
};

void y_vector_field_from_cyl(int nnod, double *phi,
                             double *v_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = y_vector_from_cyl((float) phi[i], 
                                    (float) v_cyl[i], 
                                    (float) v_cyl[i+nnod]);
    };
    return;
};


/* vector components from spherical coordinate to cylindrical coordinate */

void r_vector_field_from_cyl(int nnod, double *theta,
                             double *v_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = r_vector_from_cyl((float) theta[i],
                                    (float) v_cyl[i], 
                                    (float) v_cyl[i+2*nnod]);
    };
    return;
};

void theta_vector_field_from_cyl(int nnod, double *theta,
                             double *v_cyl, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = theta_vector_from_cyl((float) theta[i],
                                        (float) v_cyl[i], 
                                        (float) v_cyl[i+2*nnod]);
    };
    return;
};

/* vector components from cylindrical coordinate to spherical coordinate */

void s_vector_field_from_sph(int nnod, double *theta,
                             double *v_sph, float *data){
    int i;
#pragma omp parallel for
    for(i=0;i<nnod;i++){
        data[i] = s_vector_from_sph((float) theta[i],
                                    (float) v_sph[i], 
                                    (float) v_sph[i+nnod]);
    };
    return;
};

