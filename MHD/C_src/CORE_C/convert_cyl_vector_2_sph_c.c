/***********************************************************************
!!*  convert_cyl_vector_2_sph_c.c
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*   convert vector from cylindrical coordinate to spherical coordinate
!!*      vr =  vs*sin(th) + vz*cos(th)
!!*      vt =  vs*cos(th) - vz*sin(th)
!!*      vp =            vp
!!*
!!*   convert vector from spherical coordinate to cylindrical coordinate
!!*      vs =  vr*sin(th) + vt*cos(th)
!!*      vp =            vp
!!*      vz =  vr*cos(th) - vt*sin(th)
 ***********************************************************************/

#include "convert_xyz_vector_2_sph_c.h"

void sph_vector_by_cyl_c(int numnod, const double *theta, 
                         const double *v_cyl, double *v_sph){
    int i;
    double v_s, v_z;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_z = v_cyl[i+2*numnod];
        
        v_sph[i         ] =  v_s * sin(theta[i])
                           + v_z * cos(theta[i]);
        v_sph[i+1*numnod] =  v_s * cos(theta[i])
                           - v_z * sin(theta[i]);
        v_sph[i+2*numnod] = v_cyl[i+1*numnod];
    };
    return;
};

void cyl_vector_by_sph_c(int numnod, const double *theta,
                         const double *v_sph, double *v_cyl){
    int i;
    double v_r, v_t;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        
        v_cyl[i         ] =  v_r * sin(theta[i])
                           + v_t * cos(theta[i]);
        v_cyl[i+1*numnod] = v_cyl[i+2*numnod];
        v_cyl[i+2*numnod] =  v_r * cos(theta[i])
                           - v_t * sin(theta[i]);
    };
    return;
};


void r_comp_by_cyl_vector_c(int numnod, const double *theta, 
                            const double *v_cyl, double *v_r){
    int i;
    double v_s, v_z;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_z = v_cyl[i+2*numnod];
        
        v_r[i] =  v_s * sin(theta[i])
                + v_z * cos(theta[i]);
    };
    return;
};

void theta_comp_by_cyl_vector_c(int numnod, const double *theta, 
                                const double *v_cyl, double *v_t){
    int i;
    double v_s, v_z;
    for(i=0;i<numnod;i++){
        v_s = v_cyl[i         ];
        v_z = v_cyl[i+2*numnod];
        
        v_t[i] =  v_s * cos(theta[i])
                - v_z * sin(theta[i]);
    };
    return;
};


void s_comp_by_sph_vector_c(int numnod, const double *theta,
                            const double *v_sph, double *v_s){
    int i;
    double v_r, v_t;
    for(i=0;i<numnod;i++){
        v_r = v_sph[i         ];
        v_t = v_sph[i+1*numnod];
        
        v_s[i] =  v_r * sin(theta[i])
                + v_t * cos(theta[i]);
    };
    return;
};
