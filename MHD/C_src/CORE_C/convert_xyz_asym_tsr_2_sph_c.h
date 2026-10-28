/***********************************************************************
 !!*  convert_xyz_asym_tsr_2_sph_c.h
 !!*
 !!*  Created by Hiroaki Matsui on Dec. 3, 2012
 !!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 !!   urut = (as) *   (              x   *uz*ux - y   *uy*uz)
 !!   upur = (aras) * (-s*s *ux*uy + y*z *uz*ux + x*z* uy*uz)
 !!   utup = (ar) *   (   z *ux*uy + y   *uz*ux + x   *uy*uz)
 !!
 !!   usup =         uxuy
 !!   uzus = (as) * ( x* uzux - y* uyuz)
 !!   upuz = (as) * ( y* uzux + x* uyuz)
 !!
 !!   usup =         uxuy
 !!   uzus = (as) * ( x* uzux - y* uyuz)
 !!   upuz = (as) * ( y* uzux + x* uyuz)
 ***********************************************************************/
#ifndef CONVERT_XYZ_ASYM_TSR_2_SPH_C_
#define CONVERT_XYZ_ASYM_TSR_2_SPH_C_

#include <math.h>

/* prototypes */
void sph_rt_asym_tensor_by_xyz_c(int numnod, const double *phi, 
                                 const double *at_xyz, double *t_rt);
void sph_pr_asym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
            const double *at_xyz, double *t_rp);
void sph_tp_asym_tensor_by_xyz_c(int numnod, const double *theta, const double *phi, 
            const double *at_xyz, double *t_tp);

void sph_sp_asym_tensor_by_xyz_c(int numnod, const double *at_xyz, double *t_sp);
void sph_zs_asym_tensor_by_xyz_c(int numnod, const double *phi, 
                                 const double *at_xyz, double *t_zs);
void sph_pz_asym_tensor_by_xyz_c(int numnod, const double *phi, 
            const double *at_xyz, double *t_pz);

#endif /* CONVERT_XYZ_ASYM_TSR_2_SPH_C_*/
