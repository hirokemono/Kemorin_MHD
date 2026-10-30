/***********************************************************************
!!  convert_sph_tensor_2_xyz_c.c
!!
!!  Created by Hiroaki Matsui on Dec. 3, 2012
!!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!
!!  uxux = (ar*as)^2 *(x*x*s*s *urur + x*x*z*s *urut - x*y*s*r * urup
!!                   + x*x*z*s *utur + x*x*z*z *utut - x*y*z*r * utup
!!                   - x*y*s*r *upur - x*y*z*r *uput + y*y*r*r * upup)
!!  uxuy = (ar*as)^2 *(x*y*s*s *urur + x*y*z*s *urut + x*x*s*r * urup
!!                   + x*y*z*s *utur + x*y*z*z *utut + x*x*z*r * utup
!!                   - y*y*s*r *upur - y*y*z*r *uput - x*y*r*r * upup)
!!  uxuz = (ar*as)^2 *(x*z*s*s *urur - x*s*s*s *urut + 0
!!                   + x*z*z*s *utur - x*z*s*s *utut + 0
!!                   - y*z*s*r *upur + y*s*s*r *uput + 0)
!!  uyuy = (ar*as)^2 *(y*y*s*s *urur + y*y*s*z *urut + x*y*s*r * urup
!!                   + y*y*z*s *utur + y*y*z*z *utut + x*y*z*r * utup
!!                   + x*y*s*r *upur + x*y*z*r *uput + x*x*r*r * upup)
!!  uyuz = (ar*as)^2 *(y*z*s*s *urur - y*s*s*s *urut + 0
!!                   + y*z*z*s *utur - y*z*s*s *utut - 0
!!                   + x*z*s*r *upur - x*r*s*s *uput + 0)
!!  uzuz = (ar*as)^2 *(z*z*s*s *urur - z*s*s*s *urut + 0
!!                   - z*s*s*s *utur + s*s*s*s *utut + 0
!!                   + 0 + 0 + 0)
 ***********************************************************************/

#include <math.h>

#include "convert_sph_tensor_2_xyz_c.h"

void sph_xx_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xx){
    int i;
    double trr, trt, trp;
    double ttt, ttp;
    double tpp;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        trp = st_sph[i+2*numnod];
        ttt = st_sph[i+3*numnod];
        ttp = st_sph[i+4*numnod];
        tpp = st_sph[i+5*numnod];
        t_xx[i] =    trr * cos(phi[i])*cos(phi[i]) * sin(theta[i]) * sin(theta[i])
               + 2.0*trt * cos(phi[i])*cos(phi[i]) * cos(theta[i]) * sin(theta[i])
               - 2.0*trp * cos(phi[i])*sin(phi[i]) * sin(theta[i])
               +     ttt * cos(phi[i])*cos(phi[i]) * cos(theta[i]) * cos(theta[i])
               - 2.0*ttp * cos(phi[i])*sin(phi[i]) * cos(theta[i])
               +     tpp * sin(phi[i])*sin(phi[i]);
    };
    return;
};

void sph_xy_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xy){
    int i;
    double trr, trt, trp;
    double ttt, ttp;
    double tpp;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        trp = st_sph[i+2*numnod];
        ttt = st_sph[i+3*numnod];
        ttp = st_sph[i+4*numnod];
        tpp = st_sph[i+5*numnod];
        t_xy[i] =  trr *  cos(phi[i])*sin(phi[i]) * sin(theta[i])*sin(theta[i])
             + 2.0*trt *  cos(phi[i])*sin(phi[i]) * cos(theta[i])*sin(theta[i])
             +     trp * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i])) * sin(theta[i])
             +     ttt *  cos(phi[i])*sin(phi[i]) * cos(theta[i])*cos(theta[i])
             +     ttp * (cos(phi[i])*cos(phi[i]) - sin(phi[i])*sin(phi[i])) * cos(theta[i])
             -     tpp *  cos(phi[i])*sin(phi[i]);
    };
    return;
};

void sph_xz_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xz){
    int i;
    double trr, trt, trp;
    double ttt, ttp;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        trp = st_sph[i+2*numnod];
        ttt = st_sph[i+3*numnod];
        ttp = st_sph[i+4*numnod];
        t_xz[i] =  trr * cos(phi[i]) * cos(theta[i])*sin(theta[i])
                 + trt * cos(phi[i]) * (cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
                 - trp * sin(phi[i]) * cos(theta[i])
                 - ttt * cos(phi[i]) * cos(theta[i])*sin(theta[i])
                 + ttp * sin(phi[i]) * sin(theta[i]);
    };
    return;
};

void sph_yy_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_yy){
    int i;
    double trr, trt, trp;
    double ttt, ttp;
    double tpp;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        trp = st_sph[i+2*numnod];
        ttt = st_sph[i+3*numnod];
        ttp = st_sph[i+4*numnod];
        tpp = st_sph[i+5*numnod];
        t_yy[i] =    trr * sin(phi[i])*sin(phi[i]) * sin(theta[i])*sin(theta[i])
               + 2.0*trt * sin(phi[i])*sin(phi[i]) * cos(theta[i])*sin(theta[i])
               + 2.0*trp * cos(phi[i])*sin(phi[i]) * sin(theta[i])
               +     ttt * sin(phi[i])*sin(phi[i]) * cos(theta[i])*cos(theta[i])
               + 2.0*ttp * cos(phi[i])*sin(phi[i]) * cos(theta[i])
               +     tpp * cos(phi[i])*cos(phi[i]);
    };
    return;
};

void sph_yz_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_yz){
    int i;
    double trr, trt, trp;
    double ttt, ttp;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        trp = st_sph[i+2*numnod];
        ttt = st_sph[i+3*numnod];
        ttp = st_sph[i+4*numnod];
        t_yz[i] = trr * sin(phi[i]) * cos(theta[i])*sin(theta[i])
               +  trt * sin(phi[i]) * (cos(theta[i])*cos(theta[i]) - sin(theta[i])*sin(theta[i]))
               -  ttt * sin(phi[i]) * cos(theta[i])*sin(theta[i])
               +  trp * cos(phi[i]) * cos(theta[i])
               -  ttp * cos(phi[i]) * sin(theta[i]);
    };
    return;
};

void sph_zz_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_zz){
    int i;
    double trr, trt;
    double ttt;
    for(i=0;i<numnod;i++){
        trr = st_sph[i         ];
        trt = st_sph[i+1*numnod];
        ttt = st_sph[i+3*numnod];
        t_zz[i] =    trr * cos(theta[i])*cos(theta[i])
               - 2.0*trt * cos(theta[i])*sin(theta[i])
               +     ttt * sin(theta[i])*sin(theta[i]);
    };
    return;
};
