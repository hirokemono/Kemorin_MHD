/***********************************************************************
!!*  convert_sph_vector_2_xyz_c.c
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vx =  vs*cos(phi) - vp*sin(phi)
!!*      vy =  vs*sin(phi) + vp*cos(phi)
!!*      vz =             vz
 ***********************************************************************/

#include <math.h>

#include "convert_xyz_vector_2_sph_c.h"

void xyz_vector_by_sph_c(int numnod, const double *theta, const double *phi, 
                         const double *v_sph, double *v_xyz){
    int i;
    double v_r, v_t, v_p;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        v_p = v_sph[i+2*numnod];
        v_xyz[i         ] =  v_r * sin(theta[i]) * cos(phi[i])
                           + v_t * cos(theta[i]) * cos(phi[i])
                           - v_p * sin(phi[i]);
        v_xyz[i+1*numnod] =  v_r * sin(theta[i]) * sin(phi[i])
                           + v_t * cos(theta[i]) * sin(phi[i])
                           + v_p * cos(phi[i]);
        v_xyz[i+2*numnod] =  v_r * cos(theta[i])
                           - v_t * sin(theta[i]);
    };
    return;
};

void cyl_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_cyl, double *v_xyz){
    int i;
    double v_s, v_p;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_p = v_cyl[i+1*numnod];
        
        v_xyz[i         ] =  v_s * cos(phi[i])
                           - v_p * sin(phi[i]);
        v_xyz[i+1*numnod] =  v_s * sin(phi[i])
                           + v_p * cos(phi[i]);
        v_xyz[i+2*numnod] = v_cyl[i+2*numnod];
    };
    return;
};

void x_comp_by_sph_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_sph, double *v_x){
    int i;
    double v_r, v_t, v_p;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        v_p = v_sph[i+2*numnod];
        v_x[i] =  v_r * sin(theta[i]) * cos(phi[i])
                + v_t * cos(theta[i]) * cos(phi[i])
                - v_p * sin(phi[i]);
    };
    return;
};

void y_comp_by_sph_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_sph, double *v_y){
    int i;
    double v_r, v_t, v_p;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        v_p = v_sph[i+2*numnod];
        v_y[i] =  v_r * sin(theta[i]) * sin(phi[i])
                + v_t * cos(theta[i]) * sin(phi[i])
                + v_p * cos(phi[i]);
    };
    return;
};

void z_comp_by_sph_vector_c(int numnod, const double *theta, 
                            const double *v_sph, double *v_z){
    int i;
    double v_r, v_t;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        v_z[i] =  v_r * cos(theta[i])
                - v_t * sin(theta[i]);
    };
    return;
};

void x_comp_by_cyl_vector_c(int numnod, const double *phi, 
                            const double *v_cyl, double *v_x){
    int i;
    double v_s, v_p;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_p = v_cyl[i+1*numnod];
        v_x[i] =  v_s * cos(phi[i])
                - v_p * sin(phi[i]);
    };
    return;
};

void y_comp_by_cyl_vector_c(int numnod, const double *phi, 
                            const double *v_cyl, double *v_y){
    int i;
    double v_s, v_p;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_p = v_cyl[i+1*numnod];
        v_y[i] =  v_s * sin(phi[i])
                + v_p * cos(phi[i]);
    };
    return;
};
