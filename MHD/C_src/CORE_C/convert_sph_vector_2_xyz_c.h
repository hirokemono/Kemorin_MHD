/***********************************************************************
!!*  convert_sph_vector_2_xyz_c.h
!!*
!!*  Created by Hiroaki Matsui on Dec. 3, 2012
!!*  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
!!*
!!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vx =  vs*cos(phi) - vp*sin(phi)
!!*      vy =  vs*sin(phi) + vp*cos(phi)
!!*      vz =             vz
 ***********************************************************************/
#ifndef CONVERT_SPH_VECTOR_2_XYZ_C_
#define CONVERT_SPH_VECTOR_2_XYZ_C_

#include <math.h>

/* prototypes */

void xyz_vector_by_sph_c(int numnod, const double *theta, const double *phi, 
                         const double *v_sph, double *v_xyz);
void cyl_vector_by_xyz_c(int numnod, const double *theta, const double *phi, 
                         const double *v_cyl, double *v_xyz);

void x_comp_by_sph_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_sph, double *v_x);
void y_comp_by_sph_vector_c(int numnod, const double *theta, const double *phi, 
                            const double *v_sph, double *v_y);
void z_comp_by_sph_vector_c(int numnod, const double *theta, 
                            const double *v_sph, double *v_z);

void x_comp_by_cyl_vector_c(int numnod, const double *phi, 
                            const double *v_cyl, double *v_x);
void y_comp_by_cyl_vector_c(int numnod, const double *phi, 
                            const double *v_cyl, double *v_y);

#endif /* CONVERT_SPH_VECTOR_2_XYZ_C_ */
