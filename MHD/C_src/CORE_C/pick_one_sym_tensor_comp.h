/*
//  pick_one_sym_tensor_comp.h
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/
#ifndef PICK_ONE_SYM_TENSOR_COMP_
#define PICK_ONE_SYM_TENSOR_COMP_


#include "Calypso_direction_flags.h"
#include "convert_one_tensor_from_sph.h"
#include "convert_one_tensor_from_cyl.h"
#include "convert_one_tensor_from_xyz.h"

float pick_from_one_sph_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float trr, float trt, float trp,
                                   float ttt, float ttp, float tpp);
float pick_from_one_cyl_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float tss, float tsp, float tsz,
                                   float tpp, float tpz, float tzz);
float pick_from_one_xyz_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float txx, float txy, float txz,
                                   float tyy, float tyz, float tzz);

#endif /* PICK_ONE_SYM_TENSOR_COMP_ */
