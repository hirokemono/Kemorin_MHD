/*
//  tensor_select_from_xyz.frag
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#define XYZ_to_XX    0
#define XYZ_to_XY    1
#define XYZ_to_XZ    2
#define XYZ_to_YY    3
#define XYZ_to_YZ    4
#define XYZ_to_ZZ    5
#define XYZ_to_RR    6
#define XYZ_to_RT    7
#define XYZ_to_RP    8
#define XYZ_to_TT    9
#define XYZ_to_TP   10
#define XYZ_to_PP   11
#define XYZ_to_SS   12
#define XYZ_to_SP   13
#define XYZ_to_SZ   14
#define XYZ_to_PZ   15
#define XYZ_to_ZX   16
#define XYZ_to_PR   17
#define XYZ_to_ZS   18

/* select tensor component from cartesian coordinate */

float pick_from_xyz_sym_tensor(uniform int id_pick,
                               float theta, float phi,
                               vec3 sym_t1, vec3 sym_t2){
    float data = 0.0;
    if      (id_pick == XYZ_to_XX){
        data = sym_t1.x;
    }else if(id_pick == XYZ_to_XY){
        data = sym_t1.y;
    }else if(id_pick == XYZ_to_XZ){
        data = sym_t1.z;
    }else if(id_pick == XYZ_to_YY){
        data = sym_t2.x;
    }else if(id_pick == XYZ_to_YZ){
        data = sym_t2.y;
    }else if(id_pick == XYZ_to_ZZ){
        data = sym_t2.z;
        
    }else if(id_pick == XYZ_to_RR){
        data = rr_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == XYZ_to_RT){
        data = rt_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == XYZ_to_RP){
        data = rp_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick == XYZ_to_TT){
        data = tt_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y, sym_t2.z);
    }else if(id_pick == XYZ_to_TP){
        data = tp_sym_tensor_from_xyz(theta, phi,
                                      sym_t1.x, sym_t1.y, sym_t1.z,
                                      sym_t2.x, sym_t2.y);
    }else if(id_pick == XYZ_to_PP){
        data = pp_sym_tensor_from_xyz(phi, sym_t1.x, sym_t1.y, 
                                      sym_t2.x);
    }else if(id_pick == XYZ_to_SS){
        data = ss_sym_tensor_from_xyz(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == XYZ_to_SP){
        data = sp_sym_tensor_from_xyz(phi,
                                      sym_t1.x, sym_t1.y,
                                      sym_t2.x);
    }else if(id_pick == XYZ_to_SZ){
        data = sz_sym_tensor_from_xyz(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else if(id_pick == XYZ_to_PZ){
        data = pz_sym_tensor_from_xyz(phi,
                                      sym_t1.z,
                                      sym_t2.y);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_xyz_asym_tensor(uniform int id_pick,
                                float theta, float phi,
                                vec3 asym_t1){
    float data = 0.0;
    if      (id_pick == XYZ_to_XY){
        data = asym_t1.x;
    }else if(id_pick == XYZ_to_XZ){
        data = asym_t1.z;
    }else if(id_pick == XYZ_to_YZ){
        data = asym_t1.y;
        
    }else if(id_pick == XYZ_to_RT){
        data = rt_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick == XYZ_to_RP){
        data = pr_asym_tensor_from_xyz(theta, phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick == XYZ_to_TP){
        data = tp_asym_tensor_from_xyz(theta, phi, asym_t1.x, asym_t1.y, asym_t1.z);

    }else if(id_pick == XYZ_to_SP){
        data = asym_t1.x;
    }else if(id_pick == XYZ_to_SZ){
        data = zs_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else if(id_pick == XYZ_to_PZ){
        data = pz_asym_tensor_from_xyz(phi, asym_t1.x, asym_t1.y, asym_t1.z);
    }else{
        data = 0.0;
    }
    return data;
}



