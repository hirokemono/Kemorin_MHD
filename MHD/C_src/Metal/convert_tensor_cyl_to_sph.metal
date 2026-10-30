/*
//  convert_tensor_to_cyl_2_sph.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>
using namespace metal;

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float rr_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_rr = 0.0;
    t_rr =    tss * sin(theta)*sin(theta)
        + 2.0*tsz * cos(theta)*sin(theta)
        +     tzz * cos(theta)*cos(theta);
    return t_rr;
};

float rt_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_rt = 0.0;
    t_rt =  tss *  cos(theta)*sin(theta)
          + tsz * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
          - tzz *  cos(theta)*sin(theta);
    return t_rt;
};

float rp_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_rp = 0.0;
    t_rp =  tsp * sin(theta)
          + tpz * cos(theta);
    return t_rp;
};

float tt_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_tt = 0.0;
    t_tt =    tss * cos(theta)*cos(theta)
        - 2.0*tsz * cos(theta)*sin(theta)
        +     tzz * sin(theta)*sin(theta);
    return t_tt;
};

float tp_sym_tensor_from_cyl(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_tp = 0.0;
    t_tp =  tsp * cos(theta)
          - tpz * sin(theta);
    return t_tp;
};

float pp_sym_tensor_from_cyl(float tpp){
    float t_pp = tpp;
    return t_pp;
};

/* symmetric tensor from spherical coordinate to cylindrical coordinate */

float ss_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_ss = 0.0;
    t_ss =    trr * sin(theta) * sin(theta)
        + 2.0*trt * cos(theta) * sin(theta)
        +     ttt * cos(theta) * cos(theta);
    return t_ss;
};

float st_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_sp = 0.0;
    t_sp =  trp * sin(theta)
          + ttp * cos(theta);
    return t_sp;
};

float sz_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_sz = 0.0;
    t_sz =  trr *  cos(theta)*sin(theta)
          + trt * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
          - ttt *  cos(theta)*sin(theta);
    return t_sz;
};

float pp_sym_tensor_from_sph(float tpp){
    float t_pp = tpp;
    return t_pp;
};

float pz_sym_tensor_from_sph(float theta,
                             float trr, float trt, float trp,
                             float ttt, float ttp, float tpp){
    float t_pz = 0.0;
    t_pz =  trp * cos(theta)
          - ttp * sin(theta);
    return t_pz;
};

/* antisymmetric tensor from cylindrical coordinate to spherical coordinate */

float rt_asym_tensor_from_cyl(float tzs){
    float t_rt = tzs;
    return t_rt;
};

float pr_asym_tensor_from_cyl(float theta, float tsp, float tzs, float tpz){
    float t_pr = 0.0;
    t_pr =  tpz * cos(theta)
          - tsp * sin(theta);
    return t_pr;
};

float tp_asym_tensor_from_cyl(float theta, float tsp, float tzs, float tpz){
    float t_tp = 0.0;
    t_tp =  tpz * sin(theta)
          + tsp * cos(theta);
    return t_tp;
};


/* antisymmetric tensor from spherical coordinate to cylindrical coordinate */

float sp_asym_tensor_from_sph(float theta, float trt, float tpr, float ttp){
    float t_sp = 0.0;
    t_sp = -tpr * sin(theta)
          + ttp * cos(theta);
    return t_sp;
};

float zs_asym_tensor_from_sph(float trt){
    float t_zs = trt;
    return t_zs;
};

float pz_asym_tensor_from_sph(float theta, float phi,
                              float trt, float tpr, float ttp){
    float t_pz = 0.0;
    t_pz =  tpr * cos(theta)
          + ttp * sin(theta);
    return t_pz;
};
