/***********************************************************************
 *  convert_sph_tensor_2_xyz_c.h
 *
 *  Created by Hiroaki Matsui on Dec. 3, 2012
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 
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
 ***********************************************************************/

#include <math.h>

void sph_xx_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xx);
void sph_xy_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xy);
void sph_xz_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_xz);
void sph_yy_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_yy);
void sph_yz_sym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_yz);
void sph_zz_sym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
                                const double *st_sph, double *t_zz);