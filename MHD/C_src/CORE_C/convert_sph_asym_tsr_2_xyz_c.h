/***********************************************************************
!!  convert_sph_asym_tsr_2_xyz_c.h
!!
!!  Created by Hiroaki Matsui on Dec. 3, 2012
!!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!
!!   uxuy = (ar) *   (              -s *upur + z   *utup)
!!   uzux = (aras) * ( r*x *urut + y*z *upur + s*y *utup)
!!   uyuz = (aras) * (-r*y *urut + x*z *upur + s*x *utup)
!!
!!   uxuy =         usup
!!   uzux = (as) * (  x* uzus + y* upuz)
!!   uyuz = (as) * ( -y* uzus + x* upuz)
 ***********************************************************************/
#ifndef CONVERT_SPH_ASYM_TSR_2_XYZ_C_
#define CONVERT_SPH_ASYM_TSR_2_XYZ_C_

#include <math.h>

/* prototypes */
void sph_xy_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_xy);
void sph_zx_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_zx);
void sph_yz_asym_tensor_by_sph_c(int numnod, const double *theta, const double *phi, 
                                 const double *st_sph, double *t_yz);

void sph_xy_asym_tensor_by_cyl_c(int numnod, const double *st_cyl, double *t_xy);
void sph_zx_asym_tensor_by_cyl_c(int numnod, const double *phi, 
                                 const double *st_cyl, double *t_zx);
void sph_yz_asym_tensor_by_cyl_c(int numnod, const double *phi, 
                                 const double *st_cyl, double *t_yz);

#endif /* CONVERT_SPH_ASYM_TSR_2_XYZ_C_ */
