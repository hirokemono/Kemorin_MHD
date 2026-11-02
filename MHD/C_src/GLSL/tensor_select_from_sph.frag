/*
//  tensor_select_from_sph.frag
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#define SPH_to_RR    0
#define SPH_to_RT    1
#define SPH_to_RP    2
#define SPH_to_TT    3
#define SPH_to_TP    4
#define SPH_to_PP    5
#define SPH_to_SS    6
#define SPH_to_SP    7
#define SPH_to_SZ    8
#define SPH_to_PZ    9
#define SPH_to_ZZ   10
#define SPH_to_XX   11
#define SPH_to_XY   12
#define SPH_to_XZ   13
#define SPH_to_YY   14
#define SPH_to_YZ   15
#define SPH_to_PR   16
#define SPH_to_ZS   17
#define SPH_to_ZX   18

/* select tensor component from cylindrical coordinate */

float pick_from_sph_sym_tensor(uniform int id_pick,
                               float theta, float phi,
                               vec3 sym_t1, vec3 sym_t2){
    float data = 0.0;
    if      (id_pick == SPH_to_RR){
        data = sym_t1.x;
    }else if(id_pick == SPH_to_RT){
        data = sym_t1.y;
    }else if(id_pick == SPH_to_RP){
        data = sym_t1.z;
    }else if(id_pick == SPH_to_TT){
        data = sym_t2.x;
    }else if(id_pick == SPH_to_TP){
        data = sym_t2.y;
    }else if(id_pick == SPH_to_PP){
        data = sym_t2.z;

    }else if(id_pick == SPH_to_SS){
        data = ss_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == SPH_to_SP){
        data = sp_sym_tensor_from_sph(theta,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick == SPH_to_SZ){
        data = sz_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == SPH_to_PZ){
        data = pz_sym_tensor_from_sph(theta,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == SPH_to_ZZ){
        data = zz_sym_tensor_from_sph(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);

    }else if(id_pick == SPH_to_XX){
        data = xx_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == SPH_to_XY){
        data = xy_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == SPH_to_XZ){
        data = xz_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick == SPH_to_YY){
        data = yy_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == SPH_to_YZ){
        data = yz_sym_tensor_from_sph(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_sph_asym_tensor(uniform int id_pick,
                                float theta, float phi,
                                vec3 asym_t1){
    float data = 0.0;
    if      (id_pick == SPH_to_RT){
        data = asym_t1.x;
    }else if(id_pick == SPH_to_RP){
        data = asym_t1.y;
    }else if(id_pick == SPH_to_TP){
        data = asym_t1.z;

    }else if(id_pick == SPH_to_SP){
        data = sp_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);
    }else if(id_pick == SPH_to_SZ){
        data = asym_t1.x;
    }else if(id_pick == SPH_to_PZ){
        data = pz_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);

    }else if(id_pick == SPH_to_XY){
        data = xy_asym_tensor_from_sph(theta, asym_t1.y, asym_t1.z);
    }else if(id_pick == SPH_to_XZ){
        data = zx_asym_tensor_from_sph(theta, phi,
                                      asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick == SPH_to_YZ){
        data = yz_asym_tensor_from_sph(theta, phi,
                                      asym_t1.x, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}

