/***********************************************************************
!!  convert_vector_field_c.h
!!
!!  Created by Hiroaki Matsui on Oct. 30, 2026
!!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
!!
 ***********************************************************************/
#ifndef CONVERT_VECTOR_FIELD_C_
#define CONVERT_VECTOR_FIELD_C_

#include "convert_one_vector_component.h"

/* prototypes */

/* vector field to cartecian coordinate */

void xyz_vector_field_by_sph_c(int nnod, const double *theta, const double *phi, 
                               double *v_sph, float *v_xyz);
void xyz_vector_field_by_cyl_c(int nnod, const double *phi, 
                               double *v_cyl, float *v_xyz);


/* vector components from cartecian coordinate to spherical coordinate */

void r_vector_field_from_xyz(int nnod, double *theta, double *phi,
                             double *v_xyz, float *data);
void theta_vector_field_from_xyz(int nnod, double *theta, double *phi,
                                 double *v_xyz, float *data);
void phi_vector_field_from_xyz(int nnod, double *phi,
                               double *st_xyz, float *data);

/*  vector components from cartecian coordinate to cylindrical coordinate */

void s_vector_field_from_xyz(int nnod, double *phi,
                             double *v_xyz, float *data);

/* vector components from spherical coordinate to cartecian coordinate */

void x_vector_field_from_sph(int nnod, double *theta, double *phi,
                             double *v_sph, float *data);
void y_vector_field_from_sph(int nnod, double *theta, double *phi,
                             double *v_sph, float *data);
void z_vector_field_from_sph(int nnod, double *theta,
                             double *v_sph, float *data);

/* vector components from cylindrical coordinate to cartecian coordinate */

void x_vector_field_from_cyl(int nnod, double *phi,
                             double *v_cyl, float *data);
void y_vector_field_from_cyl(int nnod, double *phi,
                             double *v_cyl, float *data);

/* vector components from spherical coordinate to cylindrical coordinate */

void r_vector_field_from_cyl(int nnod, double *theta,
                             double *v_cyl, float *data);
void theta_vector_field_from_cyl(int nnod, double *theta,
                                 double *v_cyl, float *data);

/* vector components from cylindrical coordinate to spherical coordinate */
void s_vector_field_from_sph(int nnod, double *theta,
                             double *v_sph, float *data);

#endif /* CONVERT_VECTOR_FIELD_C_ */
