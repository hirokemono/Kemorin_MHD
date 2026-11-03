/***********************************************************************
 !!  pick_sym_tensor_component.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef PICK_SYM_TENSOR_COMPONENT_
#define PICK_SYM_TENSOR_COMPONENT_

#include <math.h>
#include <stdio.h>

#include "calypso_direction_flags.h"
#include "set_tensor_component_name.h"
#include "convert_one_vector_component.h"
#include "convert_tensor_field_from_xyz_c.h"
#include "convert_tensor_field_from_sph_c.h"
#include "convert_tensor_field_from_cyl_c.h"

/* prototypes */
void pick_tensor_comp_c(int icomp, int numnod, 
                        double *tensor, float *component);

void pick_sph_sym_tensor_by_sph(const int icomp, int numnod,
                                double *theta, double *phi,
                                double *tensor, float *component);
void pick_sph_sym_tensor_by_cyl(const int icomp, int numnod,
                                double *theta, double *phi,
                                double *tensor, float *component);
void pick_sph_sym_tensor_by_xyz(const int icomp, int numnod,
                                double *theta, double *phi,
                                double *tensor, float *component);

void pick_sym_tensor_component(const int id_coord, const int icomp, int numnod,
                               double *theta, double *phi,
                               double *tensor, float *component);

#endif /* PICK_SYM_TENSOR_COMPONENT_ */
