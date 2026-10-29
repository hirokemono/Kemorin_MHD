/***********************************************************************
 !!*  convert_cyl_asym_tsr_2_sph_c.c
 !!*
 !!*  Created by Hiroaki Matsui on Dec. 3, 2012
 !!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!
 !!   urut =         uzus
 !!   upur = (as) * (-s* usup + z* upuz)
 !!   utup = (as) * ( z* usup + s* upuz)
 !!
 !!   usup = (as) * (-s* upur + z* utup)
 !!   uzus =         urut 
 !!   upuz = (as) * ( z* upur + s* utup)
 ***********************************************************************/

#include <math.h>

#include "convert_cyl_asym_tsr_2_sph_c.h"

void sph_rt_asym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_rt){
    int i;
    for(i=0;i<numnod;i++){
        t_rt[i] = st_cyl[i+1*numnod];
    };
    return;
};

void sph_pr_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_pr){
    int i;
    double tsp, tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i];
        tpz = st_cyl[i+2*numnod];
        t_pr[i] =  tpz * cos(theta[i])
                 - tsp * sin(theta[i]);
    };
    return;
};

void sph_tp_asym_tensor_by_cyl_c(int numnod, const double *theta, 
                                 const double *st_cyl, double *t_tp){
    int i;
    double tsp, tpz;
    for(i=0;i<numnod;i++){
        tsp = st_cyl[i];
        tpz = st_cyl[i+2*numnod];
        t_tp[i] =  tpz * sin(theta[i])
                 + tsp * cos(theta[i]);
    };
    return;
};




void sph_sp_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sp){
    int i;
    double tpr, ttp;
    for(i=0;i<numnod;i++){
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_sp[i] = -tpr * sin(theta[i])
                 + ttp * cos(theta[i]);
    };
    return;
};

void sph_zs_asym_tensor_by_sph_c(int numnod, const double *st_sph, double *t_zs){
    int i;
    for(i=0;i<numnod;i++){
        t_zs[i] = st_sph[i];
    };
    return;
};

void sph_pz_asym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_pz){
    int i;
    double tpr, ttp;
    for(i=0;i<numnod;i++){
        tpr = st_sph[i+1*numnod];
        ttp = st_sph[i+2*numnod];
        t_pz[i] =  tpr * cos(theta[i])
                 + ttp * sin(theta[i]);
    };
    return;
};


const char * const hd_scalar =   "scalar";
const char * const hd_magitude = "magnitude";

const char * const hd_x = "X";
const char * const hd_y = "Y";
const char * const hd_z = "Z";

const char * const hd_r = "R";
const char * const hd_t = "Theta";
const char * const hd_p = "Phi";
const char * const hd_s = "S";

const char * const xyz_vector_list[] = {hd_x,
                                        hd_y,
                                        hd_z,
                                        hd_r,
                                        hd_t,
                                        hd_p,
                                        hd_s,
                                        hd_magitude,
                                        NULL};

const char * const sph_vector_list[] = {hd_r,
                                        hd_t,
                                        hd_p,
                                        hd_s,
                                        hd_z,
                                        hd_x,
                                        hd_y,
                                        hd_magitude,
                                        NULL};

const char * const cyl_vector_list[] = {hd_s,
                                        hd_p,
                                        hd_z,
                                        hd_r,
                                        hd_t,
                                        hd_x,
                                        hd_y,
                                        hd_magitude,
                                        NULL};


const char * const xyz_tensor_list[] = {hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        hd_magitude,
                                        NULL};

const char * const sph_tensor_list[] = {hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        hd_zz,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_magitude,
                                        NULL};

const char * const cyl_tensor_list[] = {hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pp,
                                        hd_pz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_magitude,
                                        NULL};


const char * const xyz_asym_tsr_list[] = {hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_magitude,
                                          NULL};

const char * const sph_asym_tsr_list[] = {hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_magitude,
                                          NULL};

const char * const cyl_asym_tsr_list[] = {hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_magitude,
                                          NULL};

