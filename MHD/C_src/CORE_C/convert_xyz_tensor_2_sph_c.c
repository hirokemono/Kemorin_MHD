/***********************************************************************
 *  convert_xyz_tensor_2_sph_c.c
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/

#include <math.h>

#include "convert_xyz_tensor_2_sph_c.h"

void sph_rr_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rr){
    int i;
    double txx, txy, txz;
    double tyy, tyz;
    double tzz;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        txz = st_xyz[i+2*numnod];
        tyy = st_xyz[i+3*numnod];
        tyz = st_xyz[i+4*numnod];
        tzz = st_xyz[i+5*numnod];
        t_rr[i] =    txx * cos(phi[i])*cos(phi[i]) * sin(theta[i])*sin(theta[i])
               + 2.0*txy * cos(phi[i])*sin(phi[i]) * sin(theta[i])*sin(theta[i])
               + 2.0*txz * cos(phi[i]) *             cos(theta[i])*sin(theta[i])
               +     tyy * sin(phi[i])*sin(phi[i]) * sin(theta[i])*sin(theta[i])
               + 2.0*tyz * sin(phi[i]) *             cos(theta[i])*sin(theta[i])
               +     tzz *                           cos(theta[i])*cos(theta[i]);
    };
    return;
};

void sph_rt_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rt){
    int i;
    double txx, txy, txz;
    double tyy, tyz;
    double tzz;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        txz = st_xyz[i+2*numnod];
        tyy = st_xyz[i+3*numnod];
        tyz = st_xyz[i+4*numnod];
        tzz = st_xyz[i+5*numnod];
        t_rt[i] =    txx * cos(phi[i])*cos(phi[i]) * cos(theta[i])*sin(theta[i])
               + 2.0*txy * cos(phi[i])*sin(phi[i]) * cos(theta[i])*sin(theta[i])
               +     txz * cos(phi[i])
                       *(cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
               +     tyy * sin(phi[i])*sin(phi[i]) * cos(theta[i])*sin(theta[i])
               +     tyz * sin(phi[i])
                       *(cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
               -     tzz *cos(theta[i])*sin(theta[i]));
    };
    return;
};

void sph_rp_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rp){
    int i;
    double txx, txy, txz;
    double tyy, tyz;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        txz = st_xyz[i+2*numnod];
        tyy = st_xyz[i+3*numnod];
        tyz = st_xyz[i+4*numnod];
        t_rp[i] = -txx *  cos(phi[i])*sin(phi[i]) * sin(theta[i])
                 + txy * sin(theta[i])
                      * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i])) 
                 - txz *  sin(phi[i]) * cos(theta[i])
                 + tyy *  cos(phi[i])*sin(phi[i]) * sin(theta[i])
                 + tyz *  cos(phi[i]) * cos(theta[i]);
    };
    return;
};

void sph_tt_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_tt){
    int i;
    double txx, txy, txz;
    double tyy, tyz;
    double tzz;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        txz = st_xyz[i+2*numnod];
        tyy = st_xyz[i+3*numnod];
        tyz = st_xyz[i+4*numnod];
        tzz = st_xyz[i+5*numnod];
        t_tt[i] =      txx * cos(phi[i])*cos(phi[i]) * cos(theta[i])*cos(theta[i])
                 + 2.0*txy * cos(phi[i])*sin(phi[i]) * cos(theta[i])*cos(theta[i])
                 - 2.0*txz * cos(phi[i]) * cos(theta[i])*sin(theta[i])
                 +     tyy * sin(phi[i])*sin(phi[i]) * cos(theta[i])*cos(theta[i])
                 - 2.0*tyz * sin(phi[i]) * cos(theta[i])*sin(theta[i])
                 +     tzz * sin(theta[i])*sin(theta[i]);
    };
    return;
};

void sph_tp_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_tp){
    int i;
    double txx, txy, txz;
    double tyy, tyz;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        txz = st_xyz[i+2*numnod];
        tyy = st_xyz[i+3*numnod];
        tyz = st_xyz[i+4*numnod];
        t_tp[i] = -txx * cos(phi[i])*sin(phi[i]) * cos(theta[i])
                 + txy * cos(theta[i])
                  * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i]))
                 + txz * sin(phi[i]) * sin(theta[i])
                 + tyy * cos(phi[i])*sin(phi[i]) * cos(theta[i]) 
                 - tyz * cos(phi[i]) * cos(theta[i]);
    };
    return;
};

void sph_pp_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_pp){
    int i;
    double txx, txy;
    double tyy;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        tyy = st_xyz[i+3*numnod];
        t_pp[i] =  txx*sin(phi[i])*sin(phi[i])
             - 2.0*txy*cos(phi[i])*sin(phi[i])
                 + tyy*cos(phi[i])*cos(phi[i]);
    };
    return;
};
