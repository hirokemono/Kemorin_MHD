/***********************************************************************
 !!  convert_cyl_tensor_2_xyz_c.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
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
#ifndef CONVERT_CYL_TENSOR_2_XYZ_C_
#define CONVERT_CYL_TENSOR_2_XYZ_C_

#include <math.h>

/* prototypes */
void sph_xx_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xx);
void sph_xy_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xy);
void sph_xz_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_xz);
void sph_yy_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_yy);
void sph_yz_sym_tensor_by_cyl_c(int numnod, const double *phi, 
                                const double *st_cyl, double *t_yz);
void sph_zz_sym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_zz);

#endif /* CONVERT_CYL_TENSOR_2_XYZ_C_ */
