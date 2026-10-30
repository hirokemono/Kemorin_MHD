/*
//  convert_tensor_to_xyz.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>
using namespace metal;

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float xx_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_xx = 0.0;
    t_xx =    trr * cos(phi)*cos(phi) * sin(theta) * sin(theta)
        + 2.0*trt * cos(phi)*cos(phi) * cos(theta) * sin(theta)
        - 2.0*trp * cos(phi)*sin(phi) * sin(theta)
        +     ttt * cos(phi)*cos(phi) * cos(theta) * cos(theta)
        - 2.0*ttp * cos(phi)*sin(phi) * cos(theta)
        +     tpp * sin(phi)*sin(phi);
    return t_xx;
};

float xy_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_xy = 0.0;
    t_xy =  trr *  cos(phi)*sin(phi) * sin(theta)*sin(theta)
      + 2.0*trt *  cos(phi)*sin(phi) * cos(theta)*sin(theta)
      +     trp * (cos(phi)*cos(phi) - sin(phi)*sin(phi)) * sin(theta)
      +     ttt *  cos(phi)*sin(phi) * cos(theta)*cos(theta)
      +     ttp * (cos(phi)*cos(phi) - sin(phi)*sin(phi)) * cos(theta)
      -     tpp *  cos(phi)*sin(phi);
    return t_xy;
};

float xz_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_xz = 0.0;
    t_xz =  trr * cos(phi) * cos(theta)*sin(theta)
          + trt * cos(phi) * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
          - trp * sin(phi) * cos(theta)
          - ttt * cos(phi) * cos(theta)*sin(theta)
          + ttp * sin(phi) * sin(theta);
    return t_xz;
};

float yy_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_yy = 0.0;
    t_yy =    trr * sin(phi)*sin(phi) * sin(theta)*sin(theta)
        + 2.0*trt * sin(phi)*sin(phi) * cos(theta)*sin(theta)
        + 2.0*trp * cos(phi)*sin(phi) * sin(theta)
        +     ttt * sin(phi)*sin(phi) * cos(theta)*cos(theta)
        + 2.0*ttp * cos(phi)*sin(phi) * cos(theta)
        +     tpp * cos(phi)*cos(phi);
    return t_yy;
};

float yz_sym_tensor_from_cyl(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_yz = 0.0;
    t_yz = trr * sin(phi) * cos(theta)*sin(theta)
        +  trt * sin(phi) * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
        -  ttt * sin(phi) * cos(theta)*sin(theta)
        +  trp * cos(phi) * cos(theta)
        -  ttp * cos(phi) * sin(theta);
    return t_yz;
};

float zz_sym_tensor_from_cyl(float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_zz = 0.0;
    t_zz =    trr * cos(theta)*cos(theta)
        - 2.0*trt * cos(theta)*sin(theta)
        +     ttt * sin(theta)*sin(theta);
    return t_pp;
};

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

float xx_sym_tensor_from_cyl(float phi,
                             float tss, float tsp, float tsz,
                             float tpp, float tpz, float tzz){
    float t_xx = 0.0;
    t_xx =    tss * cos(phi)*cos(phi)
        - 2.0*tsp * cos(phi)*sin(phi)
        +     tpp * sin(phi)*sin(phi);
    return t_xx;
};

float xy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp, float tsz,
                             float tpp, float tpz, float tzz){
    float t_xy = 0.0;
    t_xy =  tss *  cos(phi)*sin(phi)
          + tsp * (cos(phi)*cos(phi) - sin(phi)*sin(phi))
          - tpp *  cos(phi)*sin(phi);
    return t_xy;
};

float xz_sym_tensor_from_cyl(float phi,
                             float tss, float tsp, float tsz,
                             float tpp, float tpz, float tzz){
    float t_xz = 0.0;
    t_xz =  tsz * cos(phi)
          - tpz * sin(phi);
    return t_xz;
};

float yy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp, float tsz,
                             float tpp, float tpz, float tzz){
    float t_yy = 0.0;
    t_yy =    tss * sin(phi)*sin(phi)
        + 2.0*tsp * cos(phi)*sin(phi)
        +     tpp * cos(phi)*cos(phi);
    return t_yy;
};

float yz_sym_tensor_from_cyl(float phi,
                             float tss, float tsp, float tsz,
                             float tpp, float tpz, float tzz){
    float t_yz = 0.0;
    t_yz =  tsz * sin(phi)
          + tpz * cos(phi);
    return t_yz;
};

float zz_sym_tensor_from_cyl(float tzz){
    float t_zz = tzz;
    return t_zz;
};

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

float xy_asym_tensor_from_sph(float phi, float trt, float tpr, float ttp){
    float t_xy = 0.0;
    t_xy = -tpr * sin(theta)
          + ttp * cos(theta);
    return t_xy;
};

float zx_asym_tensor_from_sph(float theta, float phi,
                              float trt, float tpr, float ttp){
    float t_zx = 0.0;
    t_zx =  trt * cos(phi)
          + tpr * sin(phi) * cos(theta)
          + ttp * sin(phi) * sin(theta);
    return t_zx;
};

float yz_asym_tensor_from_sph(float theta, float phi,
                              float trt, float tpr, float ttp){
    float t_yz = 0.0;
    t_yz = -trt * sin(phi)
          + tpr * cos(phi) * cos(theta)
          + ttp * cos(phi) * sin(theta);
    return t_yz;
};

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

float xy_asym_tensor_from_cyl(float tsp){
    float t_xy = tsp;
    return t_xy;
};

float zx_asym_tensor_from_cyl(float phi, float tsp, float tzs, float tpz){
    float t_zx = 0.0;
    t_zx =  tzs * cos(phi)
          + tpz * sin(phi);
    return t_zx;
};

float yz_asym_tensor_from_cyl(float phi, float tsp, float tzs, float tpz){
    float t_yz = 0.0;
    t_yz = -tzs * sin(phi)
          + tpz * cos(phi);
    return t_yz;
};

