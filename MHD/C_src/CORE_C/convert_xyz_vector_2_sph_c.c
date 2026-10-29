/***********************************************************************
!!*  convert_xyz_vector_2_sph_c.c
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*      vr =  vx*sin(th)*cos(phi) + vy*sin(th)*sin(phi) + vz*cos(phi)
!!*      vt =  vx*cos(th)*cos(phi) + vy*cos(th)*sin(phi) - vz*sin(phi)
!!*      vp = -vx*sin(phi) + vy*cos(phi)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vs =  vx*cos(phi) + vy*sin(phi)
 ***********************************************************************/

#include <math.h>

#include "convert_xyz_vector_2_sph_c.h"

void sph_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_xyz, double *v_sph){
    int i;
    double v_x, v_y, v_z;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        v_z = v_xyz[i+2*numnod];
        v_sph[i         ] =  v_x * sin(theta[i]) * cos(phi[i])
                           + v_y * sin(theta[i]) * sin(phi[i])
                           + v_z * cos(theta[i]);
        v_sph[i+1*numnod] =  v_x * cos(theta[i]) * cos(phi[i])
                           + v_y * cos(theta[i]) * sin(phi[i])
                           - v_z * sin(phi[i]);
        v_sph[i+2*numnod] = -v_x * sin(phi[i])
                           + v_y * cos(phi[i]);
    };
    return;
};

void cyl_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_xyz, double *v_cyl){
    int i;
    double v_x, v_y;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        
        v_cyl[i         ] =  v_x * cos(phi[i])
                           + v_y * sin(phi[i]);
        v_cyl[i+1*numnod] = -v_x * sin(phi[i])
                           + v_y * cos(phi[i]);
        v_cyl[i+2*numnod] = v_xyz[i+2*numnod];
    };
    return;
};

void r_comp_by_xyz_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_xyz, double *v_r){
    int i;
    double v_x, v_y, v_z;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        v_z = v_xyz[i+2*numnod];
        v_r[i] = v_x * sin(theta[i])*cos(phi[i])
               + v_y * sin(theta[i])*sin(phi[i])
               + v_z * cos(theta[i]);
    };
    return;
};

void theta_comp_by_xyz_vector_c(int numnod, const double *theta, const double *phi, 
                                const double *v_xyz, double *v_t){
    int i;
    double v_x, v_y, v_z;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        v_z = v_xyz[i+2*numnod];
        v_t[i] =  v_x * cos(theta[i]) * cos(phi[i])
                + v_y * cos(theta[i]) * sin(phi[i])
                - v_z * sin(phi[i]);
    };
    return;
};

void phi_comp_by_xyz_vector_c(int numnod, const double *phi, 
                              const double *v_xyz, double *v_p){
    int i;
    double v_x, v_y;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        v_p[i] = -v_x * sin(phi[i])
                + v_y * cos(phi[i]);
    };
    return;
};

void s_comp_by_xyz_vector_c(int numnod, const double *phi, 
                            const double *v_xyz, double *v_s){
    int i;
    double v_x, v_y;
    for(i=0;i<numnod;i++){
        v_x = v_xyz[i         ];
        v_y = v_xyz[i+1*numnod];
        v_s[i] =  v_x * cos(phi[i])
                + v_y * sin(phi[i]);
    };
    return;
};
