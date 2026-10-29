/***********************************************************************
 !!  pick_vector_component.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef PICK_VECTOR_COMPONENT_
#define PICK_VECTOR_COMPONENT_

#include "calypso_param_c.h"
#include "convert_xyz_vector_2_sph_c.h"
#include "convert_sph_vector_2_xyz_c.h"
#include "convert_cyl_vector_2_sph_c.h"
#include "set_vector_component_name.h"

/* prototypes */

void pick_vector_comp_c(int icomp, int numnod, 
                        const double *vector, double *component);
void magnitude_of_vector_c(int numnod, const double *vector,
                           double *magnitude);

void pick_vector_component_by_sph(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component);
void pick_vector_component_by_cyl(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component);
void pick_vector_component_by_xyz(const int icomp, const int numnod,
                                  const double *theta, const double *phi,
                                  const double *vector, double *component);

void pick_vector_component(const int id_coord, const int icomp, 
                           const int numnod, const double *theta, const double *phi,
                           const double *vector, double *component);
void convert_to_cartesian_vector(const int id_coord, const int numnod, 
                                 const double *theta, const double *phi,
                                 const double *vector, double *component);

#endif /* PICK_VECTOR_COMPONENT_ */
