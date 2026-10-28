/***********************************************************************
 *  convert_cyl_tensor_2_sph_c.h
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/
#ifndef CONVERT_CYL_TENSOR_2_SPH_C_
#define CONVERT_CYL_TENSOR_2_SPH_C_

#include <math.h>

/* prototypes */
void sph_rr_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_rr);
void sph_rt_sym_tensor_by_cyl_c(int numnod, const double *theta,
                                const double *st_cyl, double *t_rt);
void sph_rp_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_rp);
void sph_tt_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_tt);
void sph_tp_sym_tensor_by_cyl_c(int numnod, const double *theta, 
                                const double *st_cyl, double *t_tp);
void sph_pp_sym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_pp);

#endif /* CONVERT_CYL_TENSOR_2_SPH_C_*/
