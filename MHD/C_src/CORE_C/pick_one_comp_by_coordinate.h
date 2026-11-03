/*
//  pick_one_comp_by_coordinate.h
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/
#ifndef PICK_ONE_COMP_BY_COORDINATE_
#define PICK_ONE_COMP_BY_COORDINATE_

#include "calypso_direction_flags.h"
#include "pick_one_vector_component.h"
#include "pick_one_sym_tensor_comp.h"
#include "pick_one_asym_tensor_comp.h"

void vector_in_xyz(const int id_coordinate,
                   float theta, float phi,
                   float *vector, float *v_xyz);

float pick_from_vector(const int id_coordinate,
                       const int id_pick,
                       double theta, double phi,
                       double *vector);

float pick_from_sym_tensor(const int id_coordinate,
                           const int id_pick,
                           double theta, double phi,
                           double *sym_t);
float pick_from_asym_tensor(const int id_coordinate,
                            const int id_pick,
                            double theta, double phi,
                            double *asym_t);

#endif /* PICK_ONE_COMP_BY_COORDINATE_ */
