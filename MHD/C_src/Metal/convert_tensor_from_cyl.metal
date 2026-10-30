/*
//  convert_tensor_from_cyl.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>

#include "Calypso_direction_flags.h"

using namespace metal;

#include "../GLSL/convert_one_tensor_from_cyl.frag"

/* select tensor component from cylindrical coordinate */

float pick_from_cyl_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2){
    float data = 0.0;
    if      (id_pick[0] == CYL_to_SS){
        data = sym_t1.x;
    }else if(id_pick[0] == CYL_to_SP){
        data = sym_t1.y;
    }else if(id_pick[0] == CYL_to_SZ){
        data = sym_t1.z;
    }else if(id_pick[0] == CYL_to_PP){
        data = sym_t2.x;
    }else if(id_pick[0] == CYL_to_PZ){
        data = sym_t2.y;
    }else if(id_pick[0] == CYL_to_ZZ){
        data = sym_t2.z;

    }else if(id_pick[0] == CYL_to_RR){
        data = rr_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick[0] == CYL_to_RT){
        data = rt_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick[0] == CYL_to_RP){
        data = rp_sym_tensor_from_cyl(theta,
                                      sym_t1.y,
                                      sym_t2.y);
    }else if(id_pick[0] == CYL_to_TT){
        data = tt_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick[0] == CYL_to_TP){
        data = tp_sym_tensor_from_cyl(theta,
                                      sym_t1.y,
                                      sym_t2.y);

    }else if(id_pick[0] == CYL_to_XX){
        data = xx_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == CYL_to_XY){
        data = xy_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == CYL_to_XZ){
        data = xz_sym_tensor_from_cyl(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick[0] == CYL_to_YY){
        data = yy_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == CYL_to_YZ){
        data = yz_sym_tensor_from_cyl(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_cyl_asym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 asym_t1){
    float data = 0.0;
    if      (id_pick[0] == CYL_to_SP){
        data = asym_t1.x;
    }else if(id_pick[0] == CYL_to_SZ){
        data = asym_t1.y;
    }else if(id_pick[0] == CYL_to_PZ){
        data = asym_t1.z;

    }else if(id_pick[0] == CYL_to_RT){
        data = asym_t1.y;
    }else if(id_pick[0] == CYL_to_RP){
        data = pr_asym_tensor_from_cyl(theta, asym_t1.x, asym_t1.z);
    }else if(id_pick[0] == CYL_to_TP){
        data = tp_asym_tensor_from_cyl(theta, asym_t1.x, asym_t1.z);

    }else if(id_pick[0] == CYL_to_XY){
        data = asym_t1.x;
    }else if(id_pick[0] == CYL_to_XZ){
        data = zx_asym_tensor_from_cyl(phi, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == CYL_to_YZ){
        data = yz_asym_tensor_from_cyl(phi, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}



