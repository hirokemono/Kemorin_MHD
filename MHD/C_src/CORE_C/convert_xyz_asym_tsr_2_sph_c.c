/***********************************************************************
 !!*  convert_xyz_asym_tsr_2_sph_c.c
 !!*
 !!*  Created by Hiroaki Matsui on Dec. 3, 2012
 !!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!   urut = (as) *   (              x   *uz*ux - y   *uy*uz)
 !!   upur = (aras) * (-s*s *ux*uy + y*z *uz*ux + x*z* uy*uz)
 !!   utup = (ar) *   (   z *ux*uy + y   *uz*ux + x   *uy*uz)
 !!
 !!   usup =         uxuy
 !!   uzus = (as) * ( x* uzux - y* uyuz)
 !!   upuz = (as) * ( y* uzux + x* uyuz)
 ***********************************************************************/

#include <math.h>

#include "convert_xyz_asym_tsr_2_sph_c.h"

void sph_rt_asym_tensor_by_xyz_c(int numnod, const double *phi, 
                                 const double *at_xyz, double *t_rt){
    int i;
    double tzx, tyz;
    for(i=0;i<numnod;i++){
        tzx = at_xyz[i+1*numnod];
        tyz = at_xyz[i+2*numnod];
        t_rt[i] =  tzx * cos(phi[i])
                 - tyz * sin(phi[i]);
    };
    return;
};

void sph_pr_asym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                 const double *at_xyz, double *t_rp){
    int i;
    double txy, tzx, tyz;
    for(i=0;i<numnod;i++){
        txy = at_xyz[i         ];
        tzx = at_xyz[i+1*numnod];
        tyz = at_xyz[i+2*numnod];
        t_rp[i] = -txy *               sin(theta[i])
                 + tzx * sin(phi[i]) * cos(theta[i])
                 + tyz * cos(phi[i]) * cos(theta[i]);
    };
    return;
};

void sph_tp_asym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                 const double *at_xyz, double *t_tp){
    int i;
    double txy, tzx, tyz;
    for(i=0;i<numnod;i++){
        txy = at_xyz[i         ];
        tzx = at_xyz[i+1*numnod];
        tyz = at_xyz[i+2*numnod];
        t_tp[i] =  txy *               cos(theta[i])
                 + tzx * sin(phi[i]) * sin(theta[i])
                 + tyz * cos(phi[i]) * sin(theta[i]);
    };
    return;
};


void sph_sp_asym_tensor_by_xyz_c(int numnod, const double *at_xyz, double *t_sp){
    int i;
    for(i=0;i<numnod;i++){
        t_sp[i] = at_xyz[i         ];
    };
    return;
};

void sph_zs_asym_tensor_by_xyz_c(int numnod, const double *phi, 
                                 const double *at_xyz, double *t_zs){
    int i;
    double tzx, tyz;
    for(i=0;i<numnod;i++){
        tzx = at_xyz[i+1*numnod];
        tyz = at_xyz[i+2*numnod];
        t_zs[i] =  tzx * cos(phi[i])
                 - tyz * sin(phi[i]);
    };
    return;
};

void sph_pz_asym_tensor_by_xyz_c(int numnod, const double *phi, 
                                 const double *at_xyz, double *t_pz){
    int i;
    double tzx, tyz;
    for(i=0;i<numnod;i++){
        tzx = at_xyz[i+1*numnod];
        tyz = at_xyz[i+2*numnod];
        t_pz[i] =  tzx * sin(phi[i])
                 + tyz * cos(phi[i]);
    };
    return;
};

