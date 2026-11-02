/*
//  pick_one_asym_tensor_comp.c
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include "pick_one_asym_tensor_comp.h"

float pick_from_one_sph_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float trt, float tpr, float ttp){
    float data = 0.0;
    if      (id_pick == SPH_to_RT){
        data = trt;
    }else if(id_pick == SPH_to_RP){
        data = tpr;
    }else if(id_pick == SPH_to_TP){
        data = ttp;

    }else if(id_pick == SPH_to_SP){
        data = sp_asym_tensor_from_sph(theta, tpr, ttp);
    }else if(id_pick == SPH_to_SZ){
        data = trt;
    }else if(id_pick == SPH_to_PZ){
        data = pz_asym_tensor_from_sph(theta, tpr, ttp);

    }else if(id_pick == SPH_to_XY){
        data = xy_asym_tensor_from_sph(theta, tpr, ttp);
    }else if(id_pick == SPH_to_XZ){
        data = zx_asym_tensor_from_sph(theta, phi,
                                      trt, tpr, ttp);
    }else if(id_pick == SPH_to_YZ){
        data = yz_asym_tensor_from_sph(theta, phi,
                                      trt, tpr, ttp);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_one_cyl_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float tsp, float tzs, float tpz){
    float data = 0.0;
    if      (id_pick == CYL_to_SP){
        data = tsp;
    }else if(id_pick == CYL_to_SZ){
        data = tzs;
    }else if(id_pick == CYL_to_PZ){
        data = tpz;

    }else if(id_pick == CYL_to_RT){
        data = tzs;
    }else if(id_pick == CYL_to_RP){
        data = pr_asym_tensor_from_cyl(theta, tsp, tpz);
    }else if(id_pick == CYL_to_TP){
        data = tp_asym_tensor_from_cyl(theta, tsp, tpz);

    }else if(id_pick == CYL_to_XY){
        data = tsp;
    }else if(id_pick == CYL_to_XZ){
        data = zx_asym_tensor_from_cyl(phi, tzs, tpz);
    }else if(id_pick == CYL_to_YZ){
        data = yz_asym_tensor_from_cyl(phi, tzs, tpz);
    }else{
        data = 0.0;
    }
    return data;
}

float pick_from_one_xyz_asym_tensor(const int id_pick,
                                    float theta, float phi,
                                    float txy, float tzx, float tyz){
    float data = 0.0;
    if      (id_pick == XYZ_to_XY){
        data = txy;
    }else if(id_pick == XYZ_to_XZ){
        data = tzx;
    }else if(id_pick == XYZ_to_YZ){
        data = tyz;
        
    }else if(id_pick == XYZ_to_RT){
        data = rt_asym_tensor_from_xyz(phi, txy, tzx, tyz);
    }else if(id_pick == XYZ_to_RP){
        data = pr_asym_tensor_from_xyz(theta, phi, txy, tzx, tyz);
    }else if(id_pick == XYZ_to_TP){
        data = tp_asym_tensor_from_xyz(theta, phi, txy, tzx, tyz);

    }else if(id_pick == XYZ_to_SP){
        data = txy;
    }else if(id_pick == XYZ_to_SZ){
        data = zs_asym_tensor_from_xyz(phi, txy, tzx, tyz);
    }else if(id_pick == XYZ_to_PZ){
        data = pz_asym_tensor_from_xyz(phi, txy, tzx, tyz);
    }else{
        data = 0.0;
    }
    return data;
}


