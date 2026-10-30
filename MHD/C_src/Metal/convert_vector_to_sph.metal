/*
//  convert_vector_to_sph.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>

#include "Calypso_direction_flags.h"

using namespace metal;

/* vector components from cartecian coordinate to spherical coordinate */

float r_vector_from_xyz(float theta, float phi, float3 v_xyz){
    float v_r = 0.0;
    v_r =  v_xyz.x * sin(theta)*cos(phi)
         + v_xyz.y * sin(theta)*sin(phi)
         + v_xyz.z * cos(theta);
    return v_r;
};

float theta_vector_from_xyz(float theta, float phi,float3 v_xyz){
    float v_t = 0.0;
    v_t =  v_xyz.x * cos(theta) * cos(phi)
         + v_xyz.y * cos(theta) * sin(phi)
         - v_xyz.z * sin(phi);
    return v_t;
};

float phi_vector_from_xyz(float theta, float phi, float3 v_xyz){
    float v_p = 0.0;
    v_p = -v_xyz.x * sin(phi)
         + v_xyz.y * cos(phi);
    return v_p;
};


/*  vector components from cartecian coordinate to cylindrical coordinate */

float s_vector_from_xyz(float phi, float3 v_xyz){
    float v_s = 0.0;
    v_s =  v_xyz.x * cos(phi)
         + v_xyz.y * sin(phi);
    return v_s;
};

/* vector components from spherical coordinate to cartecian coordinate */

float x_vector_from_sph(float theta, float phi, float3 v_sph){
    float v_x = 0.0;
    v_x =  v_sph.x * sin(theta) * cos(phi)
         + v_sph.y * cos(theta) * cos(phi)
         - v_sph.z * sin(phi);
    return v_x;
};

float y_vector_from_sph(float theta, float phi, float3 v_sph){
    float v_y = 0.0;
    v_y =  v_sph.x * sin(theta) * sin(phi)
         + v_sph.y * cos(theta) * sin(phi)
         + v_sph.z * cos(phi);
    return v_y;
};

float z_vector_from_sph(float theta, float phi, float3 v_sph){
    float v_z = 0.0;
    v_z =  v_sph.x * cos(theta)
         + v_sph.y * sin(theta);
    return v_z;
};

/* vector components from cartecian coordinate to spherical coordinate */

float x_vector_from_cyl(float phi, float3 v_cyl){
    float v_x = 0.0;
    v_x =  v_cyl.x * cos(phi)
         - v_cyl.y * sin(phi);
    return v_x;
};

float y_vector_from_cyl(float phi, float3 v_cyl){
    float v_y = 0.0;
    v_y =  v_cyl.x * sin(phi)
         + v_cyl.y * cos(phi);
    return v_y;
};

/* vector components from cylindrical coordinate to spherical coordinate */

float r_vector_from_cyl(float theta, float3 v_cyl){
    float v_r = 0.0;
    v_r =  v_cyl.x * sin(theta)
         + v_cyl.z * cos(theta);
    return v_r;
};

float theta_vector_from_cyl(float theta, float3 v_cyl){
    float v_t = 0.0;
    v_t =  v_cyl.x * cos(theta)
         - v_cyl.z * sin(theta);
    return v_t;
};

float s_vector_from_sph(float theta, float3 v_sph){
    float v_s = 0.0;
    v_s =  v_sph.x * sin(theta)
         + v_sph.y * cos(theta);
    return v_s;
};

/* vector to cartecian coordinate */

float3 xyz_vector_from_sph(float theta, float phi, float3 v_sph){
    float3 v_xyz;
    v_xyz.x = x_vector_from_sph(theta, phi, v_sph);
    v_xyz.y = y_vector_from_sph(theta, phi, v_sph);
    v_xyz.z = z_vector_from_sph(theta, phi, v_sph);
    return v_xyz;
}

float3 xyz_vector_from_cyl(float phi, float3 v_cyl){
    float3 v_xyz;
    v_xyz.x = x_vector_from_cyl(phi, v_cyl);
    v_xyz.y = y_vector_from_cyl(phi, v_cyl);
    v_xyz.z = v_cyl.z;
    return v_xyz;
}

