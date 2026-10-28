/***********************************************************************
 *
 *  convert_xyz_tensor_2_sph_c.h
 *
 *  Created by Hiroaki Matsui on Oct. 28, 2026
 *  Copyright 2012 Dept. of Earth and Planetary Sciences, Inst of Science Tokyo
 *
 ***********************************************************************/
#ifndef CONVERT_XYZ_TENSOR_2_SPH_C_
#define CONVERT_XYZ_TENSOR_2_SPH_C_

#include <math.h>

/* prototypes */
void sph_rr_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rr);
void sph_rt_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rt);
void sph_rp_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_rp);
void sph_tt_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_tt);
void sph_tp_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_xyz, double *t_tp);
void sph_pp_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_pp);

#endif /* CONVERT_XYZ_TENSOR_2_SPH_C_*/

