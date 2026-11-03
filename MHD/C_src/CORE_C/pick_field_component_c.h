/***********************************************************************
 !!  pick_field_component_c.h
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 ***********************************************************************/
#ifndef PICK_FIELD_COMPONENT_C_
#define PICK_FIELD_COMPONENT_C_

#include "calypso_param_c.h"
#include "Calypso_direction_flags.h"
#include "pick_one_comp_by_coordinate.h"
#include "pick_vector_component.h"
#include "pick_sym_tensor_component.h"
#include "pick_asym_tensor_component.h"

/* prototypes */

float pick_one_component_c(const int id_field,
                           const int id_coord, const int i_pick,
                           double theta, double phi,
                           double *vector);

void pick_field_component_c(const int id_field,  const int id_coord, const int i_pick, 
                            const int nnod, double *theta, double *phi, 
                            double *vector, float *component);

#endif /* PICK_FIELD_COMPONENT_C_ */
