/***********************************************************************
 !!  convert_sph_tensor_2_cyl_c.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/

#include <math.h>

#include "convert_sph_tensor_2_cyl_c.h"

void sph_ss_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_ss){
    int i;
    double trr, trt;
    double ttt;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        ttt = st_sph[i+3*numnod];
        t_ss[i] =    trr * sin(theta[i]) * sin(theta[i])
               + 2.0*trt * cos(theta[i]) * sin(theta[i])
               +     ttt * cos(theta[i]) * cos(theta[i]);
    };
    return;
};

void sph_sp_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sp){
    int i;
    double trp;
    double ttp;
    for(i=0;i<numnod;i++){
        trp = st_sph[i+2*numnod];
        ttp = st_sph[i+4*numnod];
        t_sp[i] =  trp * sin(theta[i])
                 + ttp * cos(theta[i]);
    };
    return;
};

void sph_sz_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sz){
    int i;
    double trr, trt;
    double ttt;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        ttt = st_sph[i+3*numnod];
        t_sz[i] = trr *  cos(theta[i])*sin(theta[i])
                + trt * (cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
                - ttt *  cos(theta[i])*sin(theta[i]);
    };
    return;
};

void sph_pp_sym_tensor_by_sph_c(int numnod, const double *st_sph, double *t_pp){
    int i;
    for(i=0;i<numnod;i++){
        t_pp[i] = st_sph[i+5*numnod];
    };
    return;
};

void sph_pz_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_pz){
    int i;
    double trp;
    double ttp;
    for(i=0;i<numnod;i++){
        trp = st_sph[i+2*numnod];
        ttp = st_sph[i+4*numnod];
        t_pz[i] =  trp * cos(theta[i])
                 - ttp * sin(theta[i]);
    };
    return;
};

/*
void sph_zz_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_zz){
    int i;
    double trr, trt;
    double ttt;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        ttt = st_sph[i+3*numnod];
        t_zz[i] =  trr * cos(theta[i])*cos(theta[i])
             - 2.0*trt * cos(theta[i])*sin(theta[i])
             +     ttt * sin(theta[i])*sin(theta[i]);
    };
    return;
};
*/