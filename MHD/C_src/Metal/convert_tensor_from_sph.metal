/*
//  convert_tensor_from_sph.metal
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include <metal_stdlib>

#include "Calypso_direction_flags.h"

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
                             float ttt, float ttp){
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

float yz_sym_tensor_from_sph(float theta, float phi,
                             float trr, float trt, float trp,
                             float ttt, float ttp){
    float t_yz = 0.0;
    t_yz = trr * sin(phi) * cos(theta)*sin(theta)
        +  trt * sin(phi) * (cos(theta)*cos(theta) - sin(theta)*sin(theta))
        -  ttt * sin(phi) * cos(theta)*sin(theta)
        +  trp * cos(phi) * cos(theta)
        -  ttp * cos(phi) * sin(theta);
    return t_yz;
};

float zz_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt){
    float t_zz = 0.0;
    t_zz =    trr * cos(theta)*cos(theta)
        - 2.0*trt * cos(theta)*sin(theta)
        +     ttt * sin(theta)*sin(theta);
    return t_zz;
};

/* symmetric tensor from spherical coordinate to cylindrical coordinate */

float ss_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt){
    float t_ss = 0.0;
    t_ss =    trr * sin(theta) * sin(theta)
        + 2.0*trt * cos(theta) * sin(theta)
        +     ttt * cos(theta) * cos(theta);
    return t_ss;
};

float sp_sym_tensor_from_sph(float theta,
                             float trp,
                             float ttp){
    float t_sp = 0.0;
    t_sp =  trp * sin(theta)
          + ttp * cos(theta);
    return t_sp;
};

float sz_sym_tensor_from_sph(float theta,
                             float trr, float trt,
                             float ttt){
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
                             float tss, float tsp, float trp,
                             float ttt, float ttp, float tpp){
    float t_pz = 0.0;
    t_pz =  trp * cos(theta)
          - ttp * sin(theta);
    return t_pz;
};

/* antisymmetric tensor from cartecian coordinate to spherical coordinate */

float xy_asym_tensor_from_sph(float theta, float tpr, float ttp){
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

/* antisymmetric tensor from spherical coordinate to cylindrical coordinate */

float sp_asym_tensor_from_sph(float theta, float tpr, float ttp){
    float t_sp = 0.0;
    t_sp = -tpr * sin(theta)
          + ttp * cos(theta);
    return t_sp;
};

float zs_asym_tensor_from_sph(float trt){
    float t_zs = trt;
    return t_zs;
};

float pz_asym_tensor_from_sph(float theta,
                              float tpr, float ttp){
    float t_pz = 0.0;
    t_pz =  tpr * cos(theta)
          + ttp * sin(theta);
    return t_pz;
};


/* select tensor component from spherical coordinate */

float pick_from_sph_sym_tensor(constant int id_pick[1],
                               float theta, float phi,
                               float3 sym_t1, float3 sym_t2){
    float data = 0.0;
    if      (id_pick[0] == SPH_to_RR){
        data = sym_t1.x;
    }else if(id_pick[0] == SPH_to_RT){
        data = sym_t1.y;
    }else if(id_pick[0] == SPH_to_RP){
        data = sym_t1.z;
    }else if(id_pick[0] == SPH_to_TT){
        data = sym_t2.x;
    }else if(id_pick[0] == SPH_to_TP){
        data = sym_t2.y;
    }else if(id_pick[0] == SPH_to_PP){
        data = sym_t2.z;

    }else if(id_pick[0] == SPH_to_SS){
        data = ss_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == SPH_to_SP){
        data = sp_sym_tensor_from_sph(theta,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick[0] == SPH_to_SZ){
        data = sz_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick[0] == SPH_to_PZ){
        data = pz_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == SPH_to_ZZ){
        data = zz_sym_tensor_from_sph(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);

    }else if(id_pick[0] == SPH_to_XX){
        data = xx_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == SPH_to_XY){
        data = xy_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == SPH_to_XZ){
        data = xz_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick[0] == SPH_to_YY){
        data = yy_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick[0] == SPH_to_YZ){
        data = yz_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_sph_asym_tensor(constant int id_pick[1],
                                float theta, float phi,
                                float3 asym_t1){
    float data = 0.0;
    if      (id_pick[0] == SPH_to_RT){
        data = asym_t1.x;
    }else if(id_pick[0] == SPH_to_RP){
        data = asym_t1.y;
    }else if(id_pick[0] == SPH_to_TP){
        data = asym_t1.z;

    }else if(id_pick[0] == SPH_to_SP){
        data = sp_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == SPH_to_SZ){
        data = asym_t1.x;
    }else if(id_pick[0] == SPH_to_PZ){
        data = pz_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);

    }else if(id_pick[0] == SPH_to_XY){
        data = xy_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == SPH_to_XZ){
        data = zx_asym_tensor_from_sph(theta, phi,
                                      asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick[0] == SPH_to_YZ){
        data = yz_asym_tensor_from_sph(theta, phi,
                                      asym_t1.x, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}

