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

/* symmetric tensor from cartecian coordinate to cylindrical coordinate */

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



