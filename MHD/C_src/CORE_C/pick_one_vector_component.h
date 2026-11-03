/*
//  pick_one_vector_component.h
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/
#ifndef PICK_ONE_VECTOR_COMPONENT_
#define PICK_ONE_VECTOR_COMPONENT_


#include "calypso_direction_flags.h"
#include "convert_one_vector_component.h"

/* vector to cartecian coordinate */

void one_xyz_vector_from_sph(float theta, float phi, 
                             float *v_sph, float *v_xyz);
void one_xyz_vector_from_cyl(float phi, float *v_cyl, float *v_xyz);

/* select vector component from spherical coordinate */

float pick_from_one_sph_vector(const int id_pick,
                               float theta, float phi,
                               float v_r, float v_t, float v_p);
float pick_from_one_cyl_vector(const int id_pick,
                               float theta, float phi,
                               float v_s, float v_p, float v_z);
float pick_from_one_xyz_vector(const int id_pick,
                               float theta, float phi,
                               float v_x, float v_y, float v_z);

#endif /* PICK_ONE_VECTOR_COMPONENT_ */
