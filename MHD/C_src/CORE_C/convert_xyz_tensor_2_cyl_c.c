/***********************************************************************
 *  convert_xyz_tensor_2_cyl_c.c
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/

#include <math.h>

#include "convert_xyz_tensor_2_cyl_c.h"

void sph_ss_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_ss){
    int i;
    double txx, txy;
    double tyy;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        tyy = st_xyz[i+3*numnod];
        t_ss[i] =    txx * cos(phi[i])*cos(phi[i])
               + 2.0*txy * cos(phi[i])*sin(phi[i])
               +     tyy * sin(phi[i])*sin(phi[i]);
    };
    return;
};

void sph_sp_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_sp){
    int i;
    double txx, txy;
    double tyy;
    for(i=0;i<numnod;i++){
        txx = st_xyz[i         ];
        txy = st_xyz[i+1*numnod];
        tyy = st_xyz[i+3*numnod];
        t_sp[i] = -txx * cos(phi[i])*sin(phi[i])
                 + txy * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i]))
                 + tyy * sin(phi[i])*cos(phi[i]);
    };
    return;
};

void sph_sz_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_sz){
    int i;
    double txz;
    double tyz;
    for(i=0;i<numnod;i++){
        txz = st_xyz[i+2*numnod];
        tyz = st_xyz[i+4*numnod];
        t_sz[i] =  txz *  cos(phi[i])
                 + tyz *  sin(phi[i]);
    };
    return;
};

void sph_pz_sym_tensor_by_xyz_c(int numnod, const double *phi, 
            const double *st_xyz, double *t_pz){
    int i;
    double txz;
    double tyz;
    for(i=0;i<numnod;i++){
        txz = st_xyz[i+2*numnod];
        tyz = st_xyz[i+4*numnod];
        t_pz[i] = -txz * sin(phi[i])
                 + tyz * cos(phi[i]);
    };
    return;
};

void sph_zz_sym_tensor_by_xyz_c(int numnod, const double *st_xyz, double *t_zz){
    int i;
    for(i=0;i<numnod;i++){
        t_zz[i] = st_xyz[i+5*numnod];
    };
    return;
};
