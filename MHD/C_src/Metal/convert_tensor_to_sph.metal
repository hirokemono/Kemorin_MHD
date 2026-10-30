/*
//  convert_tensor_to_sph.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>
using namespace metal;

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float rr_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_rr = 0.0;
    t_rr =    txx * cos(phi)*cos(phi) * sin(theta)*sin(theta)
        + 2.0*txy * cos(phi)*sin(phi) * sin(theta)*sin(theta)
        + 2.0*txz * cos(phi) *          cos(theta)*sin(theta)
        +     tyy * sin(phi)*sin(phi) * sin(theta)*sin(theta)
        + 2.0*tyz * sin(phi) *          cos(theta)*sin(theta)
        +     tzz *                     cos(theta)*cos(theta);
    return t_rr;
};

float rt_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_rt = 0.0;
    t_rt =    txx * cos(phi)*cos(phi) * cos(theta)*sin(theta)
        + 2.0*txy * cos(phi)*sin(phi) * cos(theta)*sin(theta)
        +     txz * cos(phi)
                * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
        +     tyy * sin(phi)*sin(phi) * cos(theta)*sin(theta)
        +     tyz * sin(phi)
                * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
        -     tzz *cos(theta)*sin(theta);
    return t_rt;
};

float rp_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_rp = 0.0;
    t_rp = -txx *  cos(phi)*sin(phi) * sin(theta)
          + txy * sin(theta)
               * (cos(phi)*cos(phi) - sin(phi)*sin(phi))
          - txz *  sin(phi) * cos(theta)
          + tyy *  cos(phi)*sin(phi) * sin(theta[i])
          + tyz *  cos(phi) * cos(theta);
    return t_rp;
};

float tt_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_tt = 0.0;
    t_tt =      txx * cos(phi)*cos(phi) * cos(theta)*cos(theta)
          + 2.0*txy * cos(phi)*sin(phi) * cos(theta)*cos(theta)
          - 2.0*txz * cos(phi) * cos(theta)*sin(theta)
          +     tyy * sin(phi)*sin(phi) * cos(theta)*cos(theta)
          - 2.0*tyz * sin(phi) * cos(theta)*sin(theta)
          +     tzz * sin(theta)*sin(theta);
    return t_tt;
};

float tp_sym_tensor_from_xyz(float theta, float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_tp = 0.0;
    t_tp = -txx * cos(phi)*sin(phi) * cos(theta)
          + txy * cos(theta)
           * (cos(phi)*cos(phi) - sin(phi)*sin(phi))
          + txz * sin(phi) * sin(theta)
          + tyy * cos(phi)*sin(phi) * cos(theta)
          - tyz * cos(phi) * cos(theta);
    return t_tp;
};

float pp_sym_tensor_from_xyz(float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_pp = 0.0;
    t_pp =  txx*sin(phi)*sin(phi)
      - 2.0*txy*cos(phi)*sin(phi)
          + tyy*cos(phi)*cos(phi);
    return t_pp;
};

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

float ss_sym_tensor_from_xyz(float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_ss = 0.0;
    t_ss =    txx * cos(phi)*cos(phi)
        + 2.0*txy * cos(phi)*sin(phi)
        +     tyy * sin(phi)*sin(phi);
    return t_ss;
};

float sp_sym_tensor_from_xyz(float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_sp = 0.0;
    t_sp = -txx * cos(phi)*sin(phi)
          + txy * (cos(phi)*cos(phi) - sin(phi)*sin(phi))
          + tyy * sin(phi)*cos(phi);
    return t_sp;
};

float sz_sym_tensor_from_xyz(float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_sz = 0.0;
    t_sz =  txz *  cos(phi)
          + tyz *  sin(phi);
    return t_sz;
};

float pz_sym_tensor_from_xyz(float phi,
                             float txx, float txy, float txz,
                             float tyy, float tyz, float tzz){
    float t_pz = 0.0;
    t_pz = -txz * sin(phi)
          + tyz * cos(phi);
    return t_pz;
};

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

float rt_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz){
    float t_rt = 0.0;
    t_rt =  tzx * cos(phi)
          - tyz * sin(phi);
    return t_rt;
};

float pr_asym_tensor_from_xyz(float theta, float phi,
                              float txy, float tzx, float tyz){
    float t_pr = 0.0;
    t_pr = -txy *            sin(theta)
          + tzx * sin(phi) * cos(theta)
          + tyz * cos(phi) * cos(theta);
    return t_pr;
};

float tp_asym_tensor_from_xyz(float theta, float phi,
                              float txy, float tzx, float tyz){
    float t_tp = 0.0;
    t_tp =  txy *            cos(theta)
          + tzx * sin(phi) * sin(theta)
          + tyz * cos(phi) * sin(theta);
    return t_tp;
};

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

float sp_asym_tensor_from_xyz(float txy){
    float t_sp = txy;
    return t_tp;
};

float zs_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz){
    float t_zs = 0.0;
    t_zs =  tzx * cos(phi)
          - tyz * sin(phi);
    return t_zs;
};

float pz_asym_tensor_from_xyz(float phi, float txy, float tzx, float tyz){
    float t_pz = 0.0;
    t_pz =  tzx * sin(phi)
          + tyz * cos(phi);
    return t_pz;
};

