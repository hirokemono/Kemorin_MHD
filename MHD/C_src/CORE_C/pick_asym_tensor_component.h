/***********************************************************************
 !!  pick_asym_tensor_component.h
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#ifndef PICK_ASYM_TENSOR_COMPONENT_
#define PICK_ASYM_TENSOR_COMPONENT_

#include <math.h>
#include <stdio.h>

#include "calypso_param_c.h"
#include "calypso_direction_flags.h"
#include "set_tensor_component_name.h"
#include "convert_tensor_field_from_xyz_c.h"
#include "convert_tensor_field_from_sph_c.h"
#include "convert_tensor_field_from_cyl_c.h"
#include "pick_sym_tensor_component.h"

/* prototypes */
void pick_asym_tensor_comp_by_sph(const int icomp, const int numnod,
                                  double *theta, double *phi,
                                  double *tensor, float *component);
void pick_asym_tensor_comp_by_cyl(const int icomp, const int numnod,
                                  double *theta, double *phi,
                                  double *tensor, float *component);
void pick_asym_tensor_comp_by_xyz(const int icomp, const int numnod,
                                  double *theta, double *phi,
                                  double *tensor, float *component);

void pick_asym_tensor_component(const int id_coord, const int icomp, const int numnod,
                                double *theta, double *phi,
                                double *tensor, float *component);

#endif /* PICK_ASYM_TENSOR_COMPONENT_ */
