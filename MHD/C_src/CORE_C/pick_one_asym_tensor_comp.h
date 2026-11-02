/*
//  pick_one_asym_tensor_comp.h
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/
#ifndef PICK_ONE_ASYM_TENSOR_COMP_
#define PICK_ONE_ASYM_TENSOR_COMP_


#include "Calypso_direction_flags.h"
#include "convert_one_tensor_from_sph.h"
#include "convert_one_tensor_from_cyl.h"
#include "convert_one_tensor_from_xyz.h"

float pick_from_one_sph_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float txy, float tzx, float tyz);
float pick_from_one_cyl_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float tsp, float tzs, float tpz);
float pick_from_one_xyz_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float txy, float tzx, float tyz);


#endif /* PICK_ONE_ASYM_TENSOR_COMP_ */
