/***********************************************************************
!!  convert_tensor_field_from_xyz_c.h
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
 ***********************************************************************/
#ifndef CONVERT_TENSOR_FIELD_FROM_XYZ_C_
#define CONVERT_TENSOR_FIELD_FROM_XYZ_C_

#include <math.h>
#include "convert_one_tensor_from_xyz.h"

/* prototypes */

/* symmetric tensor from cartecian coordinate to spherical coordinate */

void rr_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data);
void rt_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data);
void rp_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data);
void tt_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data);
void tp_sym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                  double *st_xyz, float *data);
void pp_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data);

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

void ss_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data);
void sp_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data);
void sz_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data);
void pz_sym_tensor_field_from_xyz(int nnod, double *phi,
                                  double *st_xyz, float *data);

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

void rt_asym_tensor_field_from_xyz(int nnod, double *phi, 
                                   double *at_xyz, float *data);
void pr_asym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                   double *at_xyz, float *data);
void tp_asym_tensor_field_from_xyz(int nnod, double *theta, double *phi,
                                   double *at_xyz, float *data);

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

void sp_asym_tensor_field_from_xyz(int nnod, double *at_xyz, float *data);
void zs_asym_tensor_field_from_xyz(int nnod, double *phi, 
                                   double *at_xyz, float *data);
void pz_asym_tensor_field_from_xyz(int nnod, double *phi, 
                                   double *at_xyz, float *data);


#endif /* CONVERT_TENSOR_FIELD_FROM_XYZ_C_ */
