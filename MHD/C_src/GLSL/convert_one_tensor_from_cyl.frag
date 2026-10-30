/***********************************************************************
 !!  convert_one_tensor_from_cyl.frag
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 ***********************************************************************/

float xx_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp){
    float t_xx = 0.0;
    t_xx =    tss * cos(phi)*cos(phi)
        - 2.0*tsp * cos(phi)*sin(phi)
        +     tpp * sin(phi)*sin(phi);
    return t_xx;
};

float xy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp){
    float t_xy = 0.0;
    t_xy =  tss *  cos(phi)*sin(phi)
          + tsp * (cos(phi)*cos(phi) - sin(phi)*sin(phi))
          - tpp *  cos(phi)*sin(phi);
    return t_xy;
};

float xz_sym_tensor_from_cyl(float phi,
                             float tsz,
                             float tpz){
    float t_xz = 0.0;
    t_xz =  tsz * cos(phi)
          - tpz * sin(phi);
    return t_xz;
};

float yy_sym_tensor_from_cyl(float phi,
                             float tss, float tsp,
                             float tpp){
    float t_yy = 0.0;
    t_yy =    tss * sin(phi)*sin(phi)
        + 2.0*tsp * cos(phi)*sin(phi)
        +     tpp * cos(phi)*cos(phi);
    return t_yy;
};

float yz_sym_tensor_from_cyl(float phi,
                             float tsz,
                             float tpz){
    float t_yz = 0.0;
    t_yz =  tsz * sin(phi)
          + tpz * cos(phi);
    return t_yz;
};

float zz_sym_tensor_from_cyl(float tzz){
    float t_zz = tzz;
    return t_zz;
};

/* symmetric tensor from cartecian coordinate to spherical coordinate */

float rr_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz){
    float t_rr = 0.0;
    t_rr =    tss * sin(theta)*sin(theta)
        + 2.0*tsz * cos(theta)*sin(theta)
        +     tzz * cos(theta)*cos(theta);
    return t_rr;
};

float rt_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz){
    float t_rt = 0.0;
    t_rt =  tss *  cos(theta)*sin(theta)
          + tsz * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
          - tzz *  cos(theta)*sin(theta);
    return t_rt;
};

float rp_sym_tensor_from_cyl(float theta,
                             float tsp,
                             float tpz){
    float t_rp = 0.0;
    t_rp =  tsp * sin(theta)
          + tpz * cos(theta);
    return t_rp;
};

float tt_sym_tensor_from_cyl(float theta,
                             float tss, float tsz,
                             float tzz){
    float t_tt = 0.0;
    t_tt =    tss * cos(theta)*cos(theta)
        - 2.0*tsz * cos(theta)*sin(theta)
        +     tzz * sin(theta)*sin(theta);
    return t_tt;
};

float tp_sym_tensor_from_cyl(float theta,
                             float tsp,
                             float tpz){
    float t_tp = 0.0;
    t_tp =  tsp * cos(theta)
          - tpz * sin(theta);
    return t_tp;
};

float pp_sym_tensor_from_cyl(float tpp){
    float t_pp = tpp;
    return t_pp;
};

/* antisymmetric tensor from cylindrical coordinate to spherical coordinate */

float rt_asym_tensor_from_cyl(float tzs){
    float t_rt = tzs;
    return t_rt;
};

float pr_asym_tensor_from_cyl(float theta, float tsp, float tpz){
    float t_pr = 0.0;
    t_pr =  tpz * cos(theta)
          - tsp * sin(theta);
    return t_pr;
};

float tp_asym_tensor_from_cyl(float theta, float tsp, float tpz){
    float t_tp = 0.0;
    t_tp =  tpz * sin(theta)
          + tsp * cos(theta);
    return t_tp;
};

/* antisymmetric tensor from cartecian coordinate to cylindrical coordinate */

float xy_asym_tensor_from_cyl(float tsp){
    float t_xy = tsp;
    return t_xy;
};

float zx_asym_tensor_from_cyl(float phi, float tzs, float tpz){
    float t_zx = 0.0;
    t_zx =  tzs * cos(phi)
          + tpz * sin(phi);
    return t_zx;
};

float yz_asym_tensor_from_cyl(float phi, float tzs, float tpz){
    float t_yz = 0.0;
    t_yz = -tzs * sin(phi)
          + tpz * cos(phi);
    return t_yz;
};

