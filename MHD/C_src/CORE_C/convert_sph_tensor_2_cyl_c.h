/***********************************************************************
 *  convert_sph_tensor_2_cyl_c.h
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/
#ifndef CONVERT_SPH_TENSOR_2_CYL_C_
#define CONVERT_SPH_TENSOR_2_CYL_C_

#include <math.h>

/* prototypes */
void sph_ss_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_ss);
void sph_sp_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sp);
void sph_sz_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_sz);
void sph_pp_sym_tensor_by_sph_c(int numnod, const double *st_sph, double *t_pp);
void sph_pz_sym_tensor_by_sph_c(int numnod, const double *theta, 
                                const double *st_sph, double *t_pz);
/*
void sph_zz_sym_tensor_by_sph_c(int numnod, const double *theta, 
            const double *st_sph, double *t_pz);
*/
#endif /* CONVERT_SPH_TENSOR_2_CYL_C_*/
