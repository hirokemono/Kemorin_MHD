/*
//  convert_vector_to_sph.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>

#include "../CORE_C/Calypso_direction_flags.h"

using namespace metal;

#include "../GLSL/convert_one_vector.frag"

/* vector to cartecian coordinate */

float3 xyz_vector_from_sph(float theta, float phi, float3 v_sph){
    float3 v_xyz;
    v_xyz.x = x_vector_from_sph(theta, phi, v_sph.x, v_sph.y, v_sph.z);
    v_xyz.y = y_vector_from_sph(theta, phi, v_sph.x, v_sph.y, v_sph.z);
    v_xyz.z = z_vector_from_sph(theta, v_sph.x, v_sph.y);
    return v_xyz;
}

float3 xyz_vector_from_cyl(float phi, float3 v_cyl){
    float3 v_xyz;
    v_xyz.x = x_vector_from_cyl(phi, v_cyl.x, v_cyl.y);
    v_xyz.y = y_vector_from_cyl(phi, v_cyl.x, v_cyl.y);
    v_xyz.z = v_cyl.z;
    return v_xyz;
}



/* select vector component from spherical coordinate */

float pick_from_sph_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_sph){
    float data = 0.0;
    if      (id_pick[0] == SPH_to_R){
        data = v_sph.x;
    }else if(id_pick[0] == SPH_to_T){
        data = v_sph.y;
    }else if(id_pick[0] == SPH_to_P){
        data = v_sph.z;

    }else if(id_pick[0] == SPH_to_S){
        data = s_vector_from_sph(theta, v_sph.x, v_sph.y);
    }else if(id_pick[0] == SPH_to_Z){
        data = z_vector_from_sph(theta, v_sph.x, v_sph.y);

    }else if(id_pick[0] == SPH_to_X){
        data = x_vector_from_sph(theta, phi, v_sph.x, v_sph.y, v_sph.z);
    }else if(id_pick[0] == SPH_to_Y){
        data = y_vector_from_sph(theta, phi, v_sph.x, v_sph.y, v_sph.z);
        
    }else if(id_pick[0] == SPH_to_MAG){
        data = length(v_sph);
    }else{
        data = 0.0;
    }
    return data;
}


float pick_from_cyl_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_cyl){
    float data = 0.0;
    if      (id_pick[0] == CYL_to_S){
        data = v_cyl.x;
    }else if(id_pick[0] == CYL_to_P){
        data = v_cyl.y;
    }else if(id_pick[0] == CYL_to_Z){
        data = v_cyl.z;

    }else if(id_pick[0] == CYL_to_R){
        data = r_vector_from_cyl(theta, v_cyl.x, v_cyl.z);
    }else if(id_pick[0] == CYL_to_T){
        data = theta_vector_from_cyl(theta, v_cyl.x, v_cyl.z);

    }else if(id_pick[0] == CYL_to_X){
        data = x_vector_from_cyl(phi, v_cyl.x, v_cyl.y);
    }else if(id_pick[0] == CYL_to_Y){
        data = y_vector_from_cyl(phi, v_cyl.x, v_cyl.y);
        
    }else if(id_pick[0] == CYL_to_MAG){
        data = length(v_cyl);
    }else{
        data = 0.0;
    }
    return data;
}


float pick_from_xyz_vector(constant int id_pick[1],
                           float theta, float phi, float3 v_xyz){
    float data = 0.0;
    if      (id_pick[0] == XYZ_to_X){
        data = v_xyz.x;
    }else if(id_pick[0] == XYZ_to_Y){
        data = v_xyz.y;
    }else if(id_pick[0] == XYZ_to_Z){
        data = v_xyz.z;

    }else if(id_pick[0] == XYZ_to_R){
        data = r_vector_from_xyz(theta, phi,
                                 v_xyz.x, v_xyz.y, v_xyz.z);
    }else if(id_pick[0] == XYZ_to_T){
        data = theta_vector_from_xyz(theta, phi,
                                     v_xyz.x, v_xyz.y, v_xyz.z);
   }else if(id_pick[0] == XYZ_to_P){
        data = phi_vector_from_xyz(phi, v_xyz.x, v_xyz.y);

    }else if(id_pick[0] == XYZ_to_S){
        data = s_vector_from_xyz(phi, v_xyz.x, v_xyz.y);
        
    }else if(id_pick[0] == XYZ_to_MAG){
        data = length(v_xyz);
    }else{
        data = 0.0;
    }
    return data;
}
