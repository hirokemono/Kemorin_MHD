/*
//  tensor_select_from_cyl.frag
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#define CYL_to_SS    0
#define CYL_to_SP    1
#define CYL_to_SZ    2
#define CYL_to_PP    3
#define CYL_to_PZ    4
#define CYL_to_ZZ    5
#define CYL_to_RR    6
#define CYL_to_RT    7
#define CYL_to_RP    8
#define CYL_to_TT    9
#define CYL_to_TP   10
#define CYL_to_XX   11
#define CYL_to_XY   12
#define CYL_to_XZ   13
#define CYL_to_YY   14
#define CYL_to_YZ   15
#define CYL_to_ZS   16
#define CYL_to_PR   17
#define CYL_to_ZX   18

/* select tensor component from cylindrical coordinate */

float pick_from_cyl_sym_tensor(uniform int id_pick,
                               float theta, float phi,
                               vec3 sym_t1, vec3 sym_t2){
    float data = 0.0;
    if      (id_pick == CYL_to_SS){
        data = sym_t1.x;
    }else if(id_pick == CYL_to_SP){
        data = sym_t1.y;
    }else if(id_pick == CYL_to_SZ){
        data = sym_t1.z;
    }else if(id_pick == CYL_to_PP){
        data = sym_t2.x;
    }else if(id_pick == CYL_to_PZ){
        data = sym_t2.y;
    }else if(id_pick == CYL_to_ZZ){
        data = sym_t2.z;

    }else if(id_pick == CYL_to_RR){
        data = rr_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick == CYL_to_RT){
        data = rt_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick == CYL_to_RP){
        data = rp_sym_tensor_from_cyl(theta,
                                      sym_t1.y,
                                      sym_t2.y);
    }else if(id_pick == CYL_to_TT){
        data = tt_sym_tensor_from_cyl(theta,
                                      sym_t1.x, sym_t1.z,
                                      sym_t2.z);
    }else if(id_pick == CYL_to_TP){
        data = tp_sym_tensor_from_cyl(theta,
                                      sym_t1.y,
                                      sym_t2.y);

    }else if(id_pick == CYL_to_XX){
        data = xx_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == CYL_to_XY){
        data = xy_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == CYL_to_XZ){
        data = xz_sym_tensor_from_cyl(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick == CYL_to_YY){
        data = yy_sym_tensor_from_cyl(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == CYL_to_YZ){
        data = yz_sym_tensor_from_cyl(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_cyl_asym_tensor(uniform int id_pick,
                               float theta, float phi,
                               vec3 asym_t1){
    float data = 0.0;
    if      (id_pick == CYL_to_SP){
        data = asym_t1.x;
    }else if(id_pick == CYL_to_SZ){
        data = asym_t1.y;
    }else if(id_pick == CYL_to_PZ){
        data = asym_t1.z;

    }else if(id_pick == CYL_to_RT){
        data = asym_t1.y;
    }else if(id_pick == CYL_to_RP){
        data = pr_asym_tensor_from_cyl(theta, asym_t1.x, asym_t1.z);
    }else if(id_pick == CYL_to_TP){
        data = tp_asym_tensor_from_cyl(theta, asym_t1.x, asym_t1.z);

    }else if(id_pick == CYL_to_XY){
        data = asym_t1.x;
    }else if(id_pick == CYL_to_XZ){
        data = zx_asym_tensor_from_cyl(phi, asym_t1.y, asym_t1.z);
    }else if(id_pick == CYL_to_YZ){
        data = yz_asym_tensor_from_cyl(phi, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}

