/***********************************************************************
!!  convert_sph_asym_tsr_2_xyz_c.c
!!
!!  Created by Hiroaki Matsui on Dec. 3, 2012
!!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!
!!   uxuy = (ar) *   (              -s *upur + z   *utup)
!!   uzux = (aras) * ( r*x *urut + y*z *upur + s*y *utup)
!!   uyuz = (aras) * (-r*y *urut + x*z *upur + s*x *utup)
!!
!!   uxuy =         usup
!!   uzux = (as) * (  x* uzus + y* upuz)
!!   uyuz = (as) * ( -y* uzus + x* upuz)
 ***********************************************************************/

#include <math.h>

#include "convert_sph_asym_tsr_2_xyz_c.h"

void sph_xy_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_xy){
    int i;
    double tpr, ttp;
    for(i=0;i<numnod;i++){
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_xy[i] = -tpr * sin(theta[i])
                 + ttp * cos(theta[i]);
    };
    return;
};

void sph_zx_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_zx){
    int i;
    double trt, tpr, ttp;
    for(i=0;i<numnod;i++){
        trt = st_sph[i];
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_zx[i] =  trt * cos(phi[i]) 
                 + tpr * sin(phi[i]) * cos(theta[i])
                 + ttp * sin(phi[i]) * sin(theta[i]);
    };
    return;
};

void sph_yz_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_yz){
    int i;
    double trt, tpr, ttp;
    for(i=0;i<numnod;i++){
        trt = st_sph[i];
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_yz[i] = -trt * sin(phi[i])
                 + tpr * cos(phi[i]) * cos(theta[i])
                 + ttp * cos(phi[i]) * sin(theta[i]);
    };
    return;
};



void sph_xy_asym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_xy){
    int i;
    for(i=0;i<numnod;i++){
        t_xy[i] = st_cyl[i];
    };
    return;
};

void sph_zx_asym_tensor_by_cyl_c(int numnod, const double *phi, 
                                 const double *st_cyl, double *t_zx){
    int i;
    double tzs, tpz;
    for(i=0;i<numnod;i++){
        tzs = st_cyl[i+1*numnod];
        tpz = st_cyl[i+2*numnod];
        t_zx[i] =  tzs * cos(phi[i])
                 + tpz * sin(phi[i]);
    };
    return;
};

void sph_yz_asym_tensor_by_cyl_c(int numnod, const double *phi, 
                                 const double *st_cyl, double *t_yz){
    int i;
    double tzs, tpz;
    for(i=0;i<numnod;i++){
        tzs = st_cyl[i+1*numnod];
        tpz = st_cyl[i+2*numnod];
        t_yz[i] = -tzs * sin(phi[i])
                 + tpz * cos(phi[i]);
    };
    return;
};
