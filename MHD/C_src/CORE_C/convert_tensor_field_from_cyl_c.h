/***********************************************************************
 !!  convert_tensor_field_from_cyl_c.h
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 !!   uxux = (as)^2 *(x*x *usus - x*y *usup
 !!                 - x*y *usup + y*y *upup)
 !!   uxuy = (as)^2 *(x*y *usus + x*x *usup
 !!                 - y*y *usup - y*x *upup)
 !!   uxuz = (as)^2 *(x*s *usuz - y*s *upuz)
 !!   uyuy = (as)^2 *(y*y *usus + x*y *usup
 !!                 + x*y *usup + x*x *upup)
 !!   uyuz = (as)^2 *(y*s *usuz + x*s *upuz)
 !!   uzuz = (as)^2 * s*s* uzuz
 ***********************************************************************/
#ifndef CONVERT_TENSOR_FIELD_FROM_CYL_
#define CONVERT_TENSOR_FIELD_FROM_CYL_

#include <math.h>

#include "convert_one_tensor_from_cyl.h"

/* prototypes */

/* symmetric tensor from cylindrical coordinate to cartesian coordinate */
void xx_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data);
void xy_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data);
void xz_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data);
void yy_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data);
void yz_sym_tensor_field_from_cyl(int nnod, double *phi,
                                  double *st_cyl, float *data);

/* symmetric tensor from cylindrical coordinate to spherical coordinate */
void rr_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data);
void rt_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data);
void rp_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data);
void tt_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data);
void tp_sym_tensor_field_from_cyl(int nnod, double *theta,
                                  double *st_cyl, float *data);

/* antisymmetric tensor from cylindrical coordinate to spherical coordinate */

void rt_asym_tensor_field_from_cyl(int nnod, double *at_cyl, float *data);
void pr_asym_tensor_field_from_cyl(int nnod, double *theta,
                                   double *at_cyl, float *data);
void tp_asym_tensor_field_from_cyl(int nnod, double *theta, 
                                   double *at_cyl, float *data);

/* antisymmetric tensor from cylindrical coordinate to cartesian coordinate */

void xy_asym_tensor_field_from_cyl(int nnod, double *at_cyl, float *data);
void zx_asym_tensor_field_from_cyl(int nnod, double *phi, 
                                   double *at_cyl, float *data);
void yz_asym_tensor_field_from_cyl(int nnod, double *phi, 
                                   double *at_cyl, float *data);

#endif /* CONVERT_TENSOR_FIELD_FROM_CYL_ */
