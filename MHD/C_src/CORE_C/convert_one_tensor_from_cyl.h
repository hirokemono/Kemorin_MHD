/***********************************************************************
 !!  convert_one_tensor_from_cyl.h
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 ***********************************************************************/
#ifndef CONVERT_ONE_TENSOR_FROM_CYL_
#define CONVERT_ONE_TENSOR_FROM_CYL_

#include <math.h>

/* prototypes */

/* symmetric tensor from cylindrical coordinate to cartesian coordinate */
float xx_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp);
float xy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp);
float xz_sym_tensor_from_cyl(float phi,
                             float tsz,
                             float tpz);
float yy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp);
float yz_sym_tensor_from_cyl(float phi,
                             float tsz,
                             float tpz);
float zz_sym_tensor_from_cyl(float tzz);

/* symmetric tensor from cylindrical coordinate to spherical coordinate */
float rr_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz);
float rt_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz);
float rp_sym_tensor_from_cyl(float theta,
                             float tsp,
                             float tpz);
float tt_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz);
float tp_sym_tensor_from_cyl(float theta,
                             float tsp,
                             float tpz);
float pp_sym_tensor_from_cyl(float tpp);

/* antisymmetric tensor from cylindrical coordinate to spherical coordinate */

float rt_asym_tensor_from_cyl(float tzs);
float pr_asym_tensor_from_cyl(float theta, float tsp, float tpz);
float tp_asym_tensor_from_cyl(float theta, float tsp, float tpz);

/* antisymmetric tensor from cylindrical coordinate to cartesian coordinate */

float xy_asym_tensor_from_cyl(float tsp);
float zx_asym_tensor_from_cyl(float phi, float tzs, float tpz);
float yz_asym_tensor_from_cyl(float phi, float tzs, float tpz);

#endif /* CONVERT_ONE_TENSOR_FROM_CYL_ */
