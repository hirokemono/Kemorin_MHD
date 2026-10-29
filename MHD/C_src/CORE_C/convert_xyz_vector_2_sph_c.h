/***********************************************************************
!!*  convert_xyz_vector_2_sph_c.h
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*      vr =  vx*sin(th)*cos(phi) + vy*sin(th)*sin(phi) + vz*cos(phi)
!!*      vt =  vx*cos(th)*cos(phi) + vy*cos(th)*sin(phi) - vz*sin(phi)
!!*      vp = -vx*sin(phi) + vy*cos(phi)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vs =  vx*cos(phi) + vy*sin(phi)
 ***********************************************************************/
#ifndef CONVERT_XYZ_VECTOR_2_SPH_C_
#define CONVERT_XYZ_VECTOR_2_SPH_C_

#include <math.h>

/* prototypes */

void sph_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_xyz, double *v_sph);
void cyl_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_xyz, double *v_cyl);

void r_comp_by_xyz_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_xyz, double *v_r);
void theta_comp_by_xyz_vector_c(int numnod, const double *theta, const double *phi, 
                                const double *v_xyz, double *v_t);
void phi_comp_by_xyz_vector_c(int numnod, const double *phi, 
                              const double *v_xyz, double *v_p);
void s_comp_by_xyz_vector_c(int numnod, const double *phi, 
                            const double *v_xyz, double *v_s);

#endif /* CONVERT_XYZ_VECTOR_2_SPH_C_ */
