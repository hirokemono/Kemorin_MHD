/***********************************************************************
!!*  convert_cyl_vector_2_sph_c.h
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*   convert vector from cylindrical coordinate to spherical coordinate
!!*      vr =  vs*sin(th) + vz*cos(th)
!!*      vt =  vs*cos(th) - vz*sin(th)
!!*      vp =            vp
!!*
!!*   convert vector from spherical coordinate to cylindrical coordinate
!!*      vs =  vr*sin(th) + vt*cos(th)
!!*      vp =            vp
!!*      vz =  vr*cos(th) - vt*sin(th)
 ***********************************************************************/
#ifndef CONVERT_CYL_VECTOR_2_SPH_C_
#define CONVERT_CYL_VECTOR_2_SPH_C_

#include <math.h>

/* prototypes */
void sph_vector_by_cyl_c(int numnod, const double *theta, 
                         const double *v_cyl, double *v_sph);
void cyl_vector_by_sph_c(int numnod, const double *theta,
                         const double *v_sph, double *v_cyl);

void r_comp_by_cyl_vector_c(int numnod, const double *theta, 
                            const double *v_cyl, double *v_r);
void theta_comp_by_cyl_vector_c(int numnod, const double *theta, 
                                const double *v_cyl, double *v_t);
void s_comp_by_sph_vector_c(int numnod, const double *theta,
                            const double *v_sph, double *v_s);

#endif /* CONVERT_CYL_VECTOR_2_SPH_C_ */
