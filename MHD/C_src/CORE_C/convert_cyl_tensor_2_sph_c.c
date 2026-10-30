/***********************************************************************
 *  convert_cyl_tensor_2_sph_c.c
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/

#include <math.h>

#include "convert_cyl_tensor_2_sph_c.h"

void sph_rr_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_rr){
    int i;
    double tss, tsz;
    double tzz;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsz = st_cyl[i+2*numnod];
        tzz = st_cyl[i+5*numnod];
        t_rr[i] =    tss * sin(theta[i])*sin(theta[i])
               + 2.0*tsz * cos(theta[i])*sin(theta[i])
               +     tzz * cos(theta[i])*cos(theta[i]);
    };
    return;
};

void sph_rt_sym_tensor_by_cyl_c(int numnod, const double *theta,
                                const double *st_cyl, double *t_rt){
    int i;
    double tss, tsz;
    double tzz;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsz = st_cyl[i+2*numnod];
        tzz = st_cyl[i+5*numnod];
        t_rt[i] =  tss * cos(theta[i])*sin(theta[i])
                 + tsz * (cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
                 - tzz * cos(theta[i])*sin(theta[i]);
    };
    return;
};

void sph_rp_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_rp){
    int i;
    double tsp;
    double tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i+1*numnod];
        tpz = st_cyl[i+4*numnod];
        t_rp[i] =  tsp * sin(theta[i])
                 + tpz * cos(theta[i]);
    };
    return;
};

void sph_tt_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_tt){
    int i;
    double tss, tsz;
    double tzz;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsz = st_cyl[i+2*numnod];
        tzz = st_cyl[i+5*numnod];
        t_tt[i] =    tss * cos(theta[i])*cos(theta[i])
               - 2.0*tsz * cos(theta[i])*sin(theta[i])
               +     tzz * sin(theta[i])*sin(theta[i]);
    };
    return;
};

void sph_tp_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_tp){
    int i;
    double tsp;
    double tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i+1*numnod];
        tpz = st_cyl[i+4*numnod];
        t_tp[i] =  tsp * cos(theta[i])
                 - tpz * sin(theta[i]);
    };
    return;
};

void sph_pp_sym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_pp){
    int i;
    for(i=0;i<numnod;i++){
        t_pp[i] = st_cyl[i+3*numnod];
    };
    return;
};
