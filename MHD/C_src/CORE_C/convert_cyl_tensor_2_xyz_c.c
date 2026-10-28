/***********************************************************************
 !!  convert_cyl_tensor_2_xyz_c.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!   uxux = (as)^2 *(x*x *usus - x*y *usup
 !!                 - x*y *usup + y*y *upup)
 !!   uxuy = (as)^2 *(x*y *usus + x*x *usup
 !!                 - y*y *usup - y*x *upup)
 !!   uxuz = (as)^2 *(x*s *usuz - y*s *upuz)
 !!   uyuy = (as)^2 *(y*y *usus + x*y *usup
 !!                 + x*y *usup + x*x *upup)
 !!   uyuz = (as)^2 *(y*s *usuz + x*s *upuz)
 !!   uzuz = (as)^2 * s*s* uzuz
 ***********************************************************************/

#include <math.h>

#include "convert_cyl_tensor_2_xyz_c.h"

void sph_xx_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xx){
    int i;
    double tss, tsp;
    double tpp;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsp = st_cyl[i+1*numnod];
        tpp = st_cyl[i+3*numnod];
        t_xx[i] =    tss * cos(phi[i])*cos(phi[i])
               - 2.0*tsp * cos(phi[i])*sin(phi[i])
               +     tpp * sin(phi[i])*sin(phi[i]);
    };
    return;
};

void sph_xy_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xy){
    int i;
    double tss, tsp;
    double tpp;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsp = st_cyl[i+1*numnod];
        tpp = st_cyl[i+3*numnod];
        t_xy[i] =  tss *  cos(phi[i])*sin(phi[i])
                 + tsp * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i]))
                 - tpp *  cos(phi[i])*sin(phi[i]);
    };
    return;
};

void sph_xz_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xz){
    int i;
    double tsz;
    double tpz;
    for(i=0;i<numnod;i++){
        tsz = st_cyl[i+2*numnod];
        tpz = st_cyl[i+4*numnod];
        t_xz[i] =  tsz * cos(phi[i])
                 - tpz * sin(phi[i]);
    };
    return;
};

void sph_yy_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_yy){
    int i;
    double tss, tsp;
    double tpp;
    for(i=0;i<numnod;i++){
        tss = st_cyl[i         ];
        tsp = st_cyl[i+1*numnod];
        tpp = st_cyl[i+3*numnod];
        t_yy[i] =    tss * sin(phi[i])*sin(phi[i])
               + 2.0*tsp * cos(phi[i])*sin(phi[i])
               +     tpp * cos(phi[i])*cos(phi[i]);
    };
    return;
};

void sph_yz_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_yz){
    int i;
    double tsz;
    double tpz;
    for(i=0;i<numnod;i++){
        tsz = st_cyl[i+2*numnod];
        tpz = st_cyl[i+4*numnod];
        t_yz[i] =  tsz * sin(phi[i])
                 + tpz * cos(phi[i]);
    };
    return;
};

void sph_zz_sym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_zz){
    int i;
    for(i=0;i<numnod;i++){
        t_zz[i] = st_cyl[i+5*numnod];
    };
    return;
};
