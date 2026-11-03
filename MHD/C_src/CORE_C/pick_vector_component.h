/***********************************************************************
 !!  pick_vector_component.h
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 ***********************************************************************/
#ifndef PICK_VECTOR_COMPONENT_
#define PICK_VECTOR_COMPONENT_

#include "calypso_param_c.h"
#include "calypso_direction_flags.h"
#include "convert_vector_field_c.h"
#include "set_vector_component_name.h"

/* prototypes */

void pick_vector_comp_c(int icomp, int nnod,
                        double *vector, float *component);
void magnitude_of_vector_c(int nnod, double *vector, float *magnitude);

void pick_vector_component_by_sph(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component);
void pick_vector_component_by_cyl(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component);
void pick_vector_component_by_xyz(const int icomp, const int nnod,
                                  double *theta, double *phi,
                                  double *vector, float *component);

void pick_vector_component(const int id_coord, const int icomp, 
                           const int nnod, double *theta, double *phi,
                           double *vector, float *component);
void convert_to_cartesian_vector(const int id_coord, const int nnod, 
                                 double *theta, double *phi,
                                 double *vector, float *v_xyz);

#endif /* PICK_VECTOR_COMPONENT_ */
