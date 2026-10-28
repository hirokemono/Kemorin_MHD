/***********************************************************************
 *  convert_xyz_tensor_2_cyl_c.h
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 ***********************************************************************/
#ifndef CONVERT_XYZ_TENSOR_2_CYL_C_
#define CONVERT_XYZ_TENSOR_2_CYL_C_

#include <math.h>

/* prototypes */
void sph_ss_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_ss);
void sph_sp_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_sp);
void sph_sz_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_sz);

void sph_pz_sym_tensor_by_xyz_c(int numnod, const double *phi, 
                                const double *st_xyz, double *t_pz);
void sph_zz_sym_tensor_by_xyz_c(int numnod, const double *st_xyz, double *t_zz);

#endif /* CONVERT_XYZ_TENSOR_2_CYL_C_ */
