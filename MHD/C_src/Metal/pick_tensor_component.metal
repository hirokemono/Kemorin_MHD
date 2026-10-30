/*
//  pick_tensor_component.metal
//  Created by Hiroaki Matsui on 10/30/26.
//
//  select tensor component to draw
*/

#include <metal_stdlib>
// Include header shared between this Metal shader code and C code executing Metal API commands
#include "KemoViewShaderTypes.h"
#include "Calypso_direction_flags.h"

float pick_from_xyz_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2);
float pick_from_xyz_asym_tensor(constant int id_pick[1],
                                float theta, float phi,
                                float3 asym_t1);

float pick_from_sph_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2);
float pick_from_sph_asym_tensor(constant int id_pick[1],
                                float theta, float phi,
                                float3 asym_t1);

float pick_from_cyl_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2);
float pick_from_cyl_asym_tensor(constant int id_pick[1],
                                float theta, float phi,
                                float3 asym_t1);


float pick_from_sph_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_sph);
float pick_from_cyl_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_cyl);
float pick_from_xyz_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_xyz);
                           
                           

float pick_from_vector(constant int id_coordinate[1],
                       constant int id_pick[1],
                       float theta, float phi, float3 vector){
    float data = 0.0;
    if      (id_coordinate[0] == SPH_COORDINATE){
        data = pick_from_sph_vector(id_pick, theta, phi, vector);
    }else if(id_coordinate[0] == CYL_COORDINATE){
        data = pick_from_cyl_vector(id_pick, theta, phi, vector);
    }else if(id_coordinate[0] == XYZ_COORDINATE){
        data = pick_from_xyz_vector(id_pick, theta, phi, vector);
    };
    return data;
}

float pick_from_sym_tensor(constant int id_coordinate[1],
                           constant int id_pick[1],
                           float theta, float phi,
                           float3 sym_t1, float3 sym_t2){
    float data = 0.0;
    if      (id_coordinate[0] == SPH_COORDINATE){
        data = pick_from_sph_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    }else if(id_coordinate[0] == CYL_COORDINATE){
        data = pick_from_cyl_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    }else if(id_coordinate[0] == XYZ_COORDINATE){
        data = pick_from_xyz_sym_tensor(id_pick, theta, phi, sym_t1, sym_t2);
    };
    return data;
}

float pick_from_asym_tensor(constant int id_coordinate[1],
                            constant int id_pick[1],
                            float theta, float phi,
                            float3 asym_t1){
    float data = 0.0;
    if      (id_coordinate[0] == SPH_COORDINATE){
        data = pick_from_sph_asym_tensor(id_pick, theta, phi, asym_t1);
    }else if(id_coordinate[0] == CYL_COORDINATE){
        data = pick_from_cyl_asym_tensor(id_pick, theta, phi, asym_t1);
    }else if(id_coordinate[0] == XYZ_COORDINATE){
        data = pick_from_xyz_asym_tensor(id_pick, theta, phi, asym_t1);
    };
    return data;
}
