/***********************************************************************
!!  convert_one_tensor_from_sph.c
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
!!     transform symmetric tensor
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
!!
!!     transform anti-symmetric tensor
!!   uxuy = (ar) *   (              -s *upur + z   *utup)
!!   uzux = (aras) * ( r*x *urut + y*z *upur + s*y *utup)
!!   uyuz = (aras) * (-r*y *urut + x*z *upur + s*x *utup)
!!
!!   uxuy =         usup
!!   uzux = (as) * (  x* uzus + y* upuz)
!!   uyuz = (as) * ( -y* uzus + x* upuz)
 ***********************************************************************/
#ifndef CONVERT_ONE_TENSOR_FROM_SPH_
#define CONVERT_ONE_TENSOR_FROM_SPH_

#include <math.h>

/* prototypes */

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float xx_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp);
float xy_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp);
float xz_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp);
float yy_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp);
float yz_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp);
float zz_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt);

/* symmetric tensor from spherical coordinate to cylindrical coordinate */

float ss_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt);
float sp_sym_tensor_from_sph(float theta,
                             float trp,
                             float ttp);
float sz_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt);
float pp_sym_tensor_from_sph(float tpp);
float pz_sym_tensor_from_sph(float theta,
                             float tss, float tsp, float trp,
                             float ttt, float ttp, float tpp);

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

float xy_asym_tensor_from_sph(float theta, float tpr, float ttp);
float zx_asym_tensor_from_sph(float theta, float phi,
                              float trt, float tpr, float ttp);
float yz_asym_tensor_from_sph(float theta, float phi,
                              float trt, float tpr, float ttp);

/* antisymmetric tensor from spherical coordinate to cylindrical coordinate */

float sp_asym_tensor_from_sph(float theta, float tpr, float ttp);
float zs_asym_tensor_from_sph(float trt);
float pz_asym_tensor_from_sph(float theta,
                              float tpr, float ttp);

#endif /* CONVERT_ONE_TENSOR_FROM_SPH_ */
