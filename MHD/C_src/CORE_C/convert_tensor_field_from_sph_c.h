/***********************************************************************
!!  convert_tensor_field_from_sph_c.h
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
#ifndef CONVERT_TENSOR_FIELD_FROM_SPH_C_
#define CONVERT_TENSOR_FIELD_FROM_SPH_C_

#include <math.h>
#include "convert_one_tensor_from_sph.h"

/* prototypes */

/* symmetric tensor from cartecian coordinate to spherical coordinate */

void xx_sym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                  double *st_sph, float *data);
void xy_sym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                  double *st_sph, float *data);
void xz_sym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                  double *st_sph, float *data);
void yy_sym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                  double *st_sph, float *data);
void yz_sym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                  double *st_sph, float *data);
void zz_sym_tensor_field_from_sph(int nnod, double *theta,
                                  double *st_sph, float *data);

/* symmetric tensor from spherical coordinate to cylindrical coordinate */

void ss_sym_tensor_field_from_sph(int nnod, double *theta,
                                  double *st_sph, float *data);
void sp_sym_tensor_field_from_sph(int nnod, double *theta,
                                  double *st_sph, float *data);
void sz_sym_tensor_field_from_sph(int nnod, double *theta,
                                  double *st_sph, float *data);
void pp_sym_tensor_field_from_sph(int nnod, double *st_sph, float *data);
void pz_sym_tensor_field_from_sph(int nnod, double *theta,
                                  double *st_sph, float *data);

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

void xy_asym_tensor_field_from_sph(int nnod, double *theta,
                                   double *at_sph, float *data);
void zx_asym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                   double *at_sph, float *data);
void yz_asym_tensor_field_from_sph(int nnod, double *theta, double *phi,
                                   double *at_sph, float *data);

/* antisymmetric tensor from spherical coordinate to cylindrical coordinate */

void sp_asym_tensor_field_from_sph(int nnod, double *theta,
                                   double *at_sph, float *data);
void zs_asym_tensor_field_from_sph(int nnod, double *at_sph, float *data);
void pz_asym_tensor_field_from_sph(int nnod, double *theta,
                                   double *at_sph, float *data);

#endif /* CONVERT_TENSOR_FIELD_FROM_SPH_C_ */
