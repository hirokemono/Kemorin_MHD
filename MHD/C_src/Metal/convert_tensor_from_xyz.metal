/*
//  convert_tensor_from_xyz.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>

#include "Calypso_direction_flags.h"

using namespace metal;

/* symmetric tensor from cartecian coordinate to spherical coordinate */

#include "../GLSL/convert_one_tensor_from_xyz.frag"

/* select tensor component from cartesian coordinate */

float pick_from_xyz_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2){
    float data = 0.0;
    if      (id_pick[0] == XYZ_to_XX){
        data = sym_t1.x;
    }else if(id_pick[0] == XYZ_to_XY){
        data = sym_t1.y;
    }else if(id_pick[0] == XYZ_to_XZ){
        data = sym_t1.z;
    }else if(id_pick[0] == XYZ_to_YY){
        data = sym_t2.x;
    }else if(id_pick[0] == XYZ_to_YZ){
        data = sym_t2.y;
    }else if(id_pick[0] == XYZ_to_ZZ){
        data = sym_t2.z;
        
    }else if(id_pick[0] == XYZ_to_RR){
        data = rr_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == XYZ_to_RT){
        data = rt_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == XYZ_to_RP){
        data = rp_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick[0] == XYZ_to_TT){
        data = tt_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == XYZ_to_TP){
        data = tp_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick[0] == XYZ_to_PP){
        data = pp_sym_tensor_from_xyz(phi, sym_t1.x, sym_t1.y, 
                                      sym_t2.x);
    }else if(id_pick[0] == XYZ_to_SS){
        data = ss_sym_tensor_from_xyz(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == XYZ_to_SP){
        data = sp_sym_tensor_from_xyz(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == XYZ_to_SZ){
        data = sz_sym_tensor_from_xyz(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick[0] == XYZ_to_PZ){
        data = pz_sym_tensor_from_xyz(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_xyz_asym_tensor(constant int id_pick[1],
                                float theta, float phi,
                                float3 asym_t1){
    float data = 0.0;
    if      (id_pick[0] == XYZ_to_XY){
        data = asym_t1.x;
    }else if(id_pick[0] == XYZ_to_XZ){
        data = asym_t1.z;
    }else if(id_pick[0] == XYZ_to_YZ){
        data = asym_t1.y;
        
    }else if(id_pick[0] == XYZ_to_RT){
        data = rt_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == XYZ_to_RP){
        data = pr_asym_tensor_from_xyz(theta, phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == XYZ_to_TP){
        data = tp_asym_tensor_from_xyz(theta, phi, asym_t1.x, asym_t1.y, asym_t1.z);

    }else if(id_pick[0] == XYZ_to_SP){
        data = asym_t1.x;
    }else if(id_pick[0] == XYZ_to_SZ){
        data = zs_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == XYZ_to_PZ){
        data = pz_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}



