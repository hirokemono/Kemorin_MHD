/*
//  pick_one_sym_tensor_comp.c
//
//
//  Created by Hiroaki Matsui on 10/30/26.
//
*/

#include "pick_one_sym_tensor_comp.h"

/* select tensor component from spherical coordinate */

float pick_from_one_sph_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float trr, float trt, float trp,
                                   float ttt, float ttp, float tpp){
    float data = 0.0;
    if      (id_pick == SPH_to_RR){
        data = trr;
    }else if(id_pick == SPH_to_RT){
        data = trt;
    }else if(id_pick == SPH_to_RP){
        data = trp;
    }else if(id_pick == SPH_to_TT){
        data = ttt;
    }else if(id_pick == SPH_to_TP){
        data = ttp;
    }else if(id_pick == SPH_to_PP){
        data = tpp;

    }else if(id_pick == SPH_to_SS){
        data = ss_sym_tensor_from_sph(theta,
                                      trr, trt,
                                      ttt);
    }else if(id_pick == SPH_to_SP){
        data = sp_sym_tensor_from_sph(theta,
                                      trp,
                                      ttp);
    }else if(id_pick == SPH_to_SZ){
        data = sz_sym_tensor_from_sph(theta,
                                      trr, trt,
                                      ttt);
    }else if(id_pick == SPH_to_PZ){
        data = pz_sym_tensor_from_sph(theta,
                                      trr, trt, trp,
                                      ttt, ttp, tpp);
    }else if(id_pick == SPH_to_ZZ){
        data = zz_sym_tensor_from_sph(phi,
                                      trr, trt,
                                      ttt);

    }else if(id_pick == SPH_to_XX){
        data = xx_sym_tensor_from_sph(theta, phi,
                                      trr, trt, trp,
                                      ttt, ttp, tpp);
    }else if(id_pick == SPH_to_XY){
        data = xy_sym_tensor_from_sph(theta, phi,
                                      trr, trt, trp,
                                      ttt, ttp, tpp);
    }else if(id_pick == SPH_to_XZ){
        data = xz_sym_tensor_from_sph(theta, phi,
                                      trr, trt, trp,
                                      ttt, ttp);
    }else if(id_pick == SPH_to_YY){
        data = yy_sym_tensor_from_sph(theta, phi,
                                      trr, trt, trp,
                                      ttt, ttp, tpp);
    }else if(id_pick == SPH_to_YZ){
        data = yz_sym_tensor_from_sph(theta, phi,
                                      trr, trt, trp,
                                      ttt, ttp);
    }else{
        data = 0.0;
    }
    return data;
}

/* select tensor component from cylindrical coordinate */

float pick_from_one_cyl_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float tss, float tsp, float tsz,
                                   float tpp, float tpz, float tzz){
    float data = 0.0;
    if      (id_pick == CYL_to_SS){
        data = tss;
    }else if(id_pick == CYL_to_SP){
        data = tsp;
    }else if(id_pick == CYL_to_SZ){
        data = tsz;
    }else if(id_pick == CYL_to_PP){
        data = tpp;
    }else if(id_pick == CYL_to_PZ){
        data = tpz;
    }else if(id_pick == CYL_to_ZZ){
        data = tzz;

    }else if(id_pick == CYL_to_RR){
        data = rr_sym_tensor_from_cyl(theta,
                                      tss, tsz,
                                      tzz);
    }else if(id_pick == CYL_to_RT){
        data = rt_sym_tensor_from_cyl(theta,
                                      tss, tsz,
                                      tzz);
    }else if(id_pick == CYL_to_RP){
        data = rp_sym_tensor_from_cyl(theta,
                                      tsp,
                                      tpz);
    }else if(id_pick == CYL_to_TT){
        data = tt_sym_tensor_from_cyl(theta,
                                      tss, tsz,
                                      tzz);
    }else if(id_pick == CYL_to_TP){
        data = tp_sym_tensor_from_cyl(theta,
                                      tsp,
                                      tpz);

    }else if(id_pick == CYL_to_XX){
        data = xx_sym_tensor_from_cyl(phi,
                                      tss, tsp,
                                      tpp);
    }else if(id_pick == CYL_to_XY){
        data = xy_sym_tensor_from_cyl(phi,
                                      tss, tsp,
                                      tpp);
    }else if(id_pick == CYL_to_XZ){
        data = xz_sym_tensor_from_cyl(phi,
                                      tsz,
                                      tpz);
    }else if(id_pick == CYL_to_YY){
        data = yy_sym_tensor_from_cyl(phi,
                                      tss, tsp,
                                      tpp);
    }else if(id_pick == CYL_to_YZ){
        data = yz_sym_tensor_from_cyl(phi,
                                      tsz,
                                      tpz);
    }else{
        data = 0.0;
    }
    return data;
}

/* select tensor component from cartesian coordinate */

float pick_from_one_xyz_sym_tensor(const int id_pick,
                                   float theta, float phi,
                                   float txx, float txy, float txz,
                                   float tyy, float tyz, float tzz){
    float data = 0.0;
    if      (id_pick == XYZ_to_XX){
        data = txx;
    }else if(id_pick == XYZ_to_XY){
        data = txy;
    }else if(id_pick == XYZ_to_XZ){
        data = txz;
    }else if(id_pick == XYZ_to_YY){
        data = tyy;
    }else if(id_pick == XYZ_to_YZ){
        data = tyz;
    }else if(id_pick == XYZ_to_ZZ){
        data = tzz;
        
    }else if(id_pick == XYZ_to_RR){
        data = rr_sym_tensor_from_xyz(theta, phi,
                                      txx, txy, txz,
                                      tyy, tyz, tzz);
    }else if(id_pick == XYZ_to_RT){
        data = rt_sym_tensor_from_xyz(theta, phi,
                                      txx, txy, txz,
                                      tyy, tyz, tzz);
    }else if(id_pick == XYZ_to_RP){
        data = rp_sym_tensor_from_xyz(theta, phi,
                                      txx, txy, txz,
                                      tyy, tyz);
    }else if(id_pick == XYZ_to_TT){
        data = tt_sym_tensor_from_xyz(theta, phi,
                                      txx, txy, txz,
                                      tyy, tyz, tzz);
    }else if(id_pick == XYZ_to_TP){
        data = tp_sym_tensor_from_xyz(theta, phi,
                                      txx, txy, txz,
                                      tyy, tyz);
    }else if(id_pick == XYZ_to_PP){
        data = pp_sym_tensor_from_xyz(phi, txx, txy, 
                                      tyy);
    }else if(id_pick == XYZ_to_SS){
        data = ss_sym_tensor_from_xyz(phi,
                                      txx, txy,
                                      tyy);
    }else if(id_pick == XYZ_to_SP){
        data = sp_sym_tensor_from_xyz(phi,
                                      txx, txy,
                                      tyy);
    }else if(id_pick == XYZ_to_SZ){
        data = sz_sym_tensor_from_xyz(phi,
                                      txz,
                                      tyz);
    }else if(id_pick == XYZ_to_PZ){
        data = pz_sym_tensor_from_xyz(phi,
                                      txz,
                                      tyz);
    }else{
        data = 0.0;
    }
    return data;
}

