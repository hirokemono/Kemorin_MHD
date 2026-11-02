/***********************************************************************
 !!  convert_one_tensor_from_xyz.h
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 ***********************************************************************/
#ifndef CONVERT_ONE_TENSOR_FROM_XYZ_C_
#define CONVERT_ONE_TENSOR_FROM_XYZ_C_

#include <math.h>

/* prototypes */

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float rr_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz);
float rt_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz);
float rp_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz);
float tt_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz);
float tp_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz);
float pp_sym_tensor_from_xyz(float phi,
                             float txx, float txy,
                             float tyy);

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

float ss_sym_tensor_from_xyz(float phi,
                             float txx, float txy,
                             float tyy);
float sp_sym_tensor_from_xyz(float phi,
                             float txx, float txy,
                             float tyy);
float sz_sym_tensor_from_xyz(float phi,
                             float txz,
                             float tyz);
float pz_sym_tensor_from_xyz(float phi,
                             float txz,
                             float tyz);

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

float rt_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz);
float pr_asym_tensor_from_xyz(float theta, float phi,
                              float txy, float tzx, float tyz);
float tp_asym_tensor_from_xyz(float theta, float phi,
                              float txy, float tzx, float tyz);

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

float sp_asym_tensor_from_xyz(float txy);
float zs_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz);
float pz_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz);

#endif /* CONVERT_ONE_TENSOR_FROM_XYZ_C_ */
