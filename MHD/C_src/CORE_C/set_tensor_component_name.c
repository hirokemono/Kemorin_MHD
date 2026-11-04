/***********************************************************************
 !!  set_tensor_component_name.c
 !!
 !!  Created by Hiroaki Matsui on Dec. 3, 2012
 !!  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley.
 !!
 ***********************************************************************/
#include "set_tensor_component_name.h"

const char * const hd_xx = "XX";
const char * const hd_xy = "XY";
const char * const hd_xz = "XZ";
const char * const hd_yy = "YY";
const char * const hd_yz = "YZ";
const char * const hd_zz = "ZZ";
const char * const hd_zx = "ZX";

const char * const hd_rr = "RR";
const char * const hd_rt = "Rθ";
const char * const hd_rp = "Rφ";
const char * const hd_tt = "θθ";
const char * const hd_tp = "θφ";
const char * const hd_pp = "φφ";
const char * const hd_pr = "φR";

const char * const hd_ss = "SS";
const char * const hd_sp = "Sφ";
const char * const hd_sz = "SZ";
const char * const hd_pz = "φZ";
const char * const hd_zs = "ZS";


/*
const char * const hd_rr = "RR";
const char * const hd_rt = "R-Theta";
const char * const hd_rp = "R-Phi";
const char * const hd_tt = "Theta-Theta";
const char * const hd_tp = "Theta-Phi";
const char * const hd_pp = "Phi-Phi";
const char * const hd_pr = "Phi-R";

const char * const hd_ss = "SS";
const char * const hd_sp = "S-Phi";
const char * const hd_sz = "SZ";
const char * const hd_pz = "Phi-Z";
const char * const hd_zs = "ZS";
*/

const char * const xyz_tensor_list[] = {hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        NULL};

const char * const sph_tensor_list[] = {hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_pp,
                                        hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pz,
                                        hd_zz,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        NULL};

const char * const cyl_tensor_list[] = {hd_ss,
                                        hd_sp,
                                        hd_sz,
                                        hd_pp,
                                        hd_pz,
                                        hd_zz,
                                        hd_rr,
                                        hd_rt,
                                        hd_rp,
                                        hd_tt,
                                        hd_tp,
                                        hd_xx,
                                        hd_xy,
                                        hd_xz,
                                        hd_yy,
                                        hd_yz,
                                        NULL};


const char * const xyz_asym_tsr_list[] = {hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          NULL};

const char * const sph_asym_tsr_list[] = {hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          NULL};

const char * const cyl_asym_tsr_list[] = {hd_sp,
                                          hd_zs,
                                          hd_pz,
                                          hd_rt,
                                          hd_pr,
                                          hd_tp,
                                          hd_xy,
                                          hd_zx,
                                          hd_yz,
                                          NULL};


int find_comp_id_from_by_sph_tensor(const char *selected){
    if     (cmp_no_case_c(selected, hd_rr) != 0){return  SPH_to_RR;}
    else if(cmp_no_case_c(selected, hd_rt) != 0){return  SPH_to_RT;}
    else if(cmp_no_case_c(selected, hd_rp) != 0){return  SPH_to_RP;}
    else if(cmp_no_case_c(selected, hd_tt) != 0){return  SPH_to_TT;}
    else if(cmp_no_case_c(selected, hd_tp) != 0){return  SPH_to_TP;}
    else if(cmp_no_case_c(selected, hd_pp) != 0){return  SPH_to_PP;}
    
    else if(cmp_no_case_c(selected, hd_ss) != 0){return  SPH_to_SS;}
    else if(cmp_no_case_c(selected, hd_sp) != 0){return  SPH_to_SP;}
    else if(cmp_no_case_c(selected, hd_sz) != 0){return  SPH_to_SZ;}
    else if(cmp_no_case_c(selected, hd_pz) != 0){return  SPH_to_PZ;}
    else if(cmp_no_case_c(selected, hd_zz) != 0){return  SPH_to_ZZ;}
    
    else if(cmp_no_case_c(selected, hd_xx) != 0){return SPH_to_XX;}
    else if(cmp_no_case_c(selected, hd_xy) != 0){return SPH_to_XY;}
    else if(cmp_no_case_c(selected, hd_xz) != 0){return SPH_to_XZ;}
    else if(cmp_no_case_c(selected, hd_yy) != 0){return SPH_to_YY;}
    else if(cmp_no_case_c(selected, hd_yz) != 0){return SPH_to_YZ;}
    
    else if(cmp_no_case_c(selected, hd_rp) != 0){return SPH_to_PR;}
    else if(cmp_no_case_c(selected, hd_zs) != 0){return SPH_to_ZS;}
    else if(cmp_no_case_c(selected, hd_zx) != 0){return SPH_to_ZX;}
    return -1;
}

void set_component_name_by_sph_tensor(int icomp, char *comp_name){
    if     (icomp == SPH_to_RR){sprintf(comp_name, hd_rr);}
    else if(icomp == SPH_to_RT){sprintf(comp_name, hd_rt);}
    else if(icomp == SPH_to_RP){sprintf(comp_name, hd_rp);}
    else if(icomp == SPH_to_TT){sprintf(comp_name, hd_tt);}
    else if(icomp == SPH_to_TP){sprintf(comp_name, hd_tp);}
    else if(icomp == SPH_to_PP){sprintf(comp_name, hd_pp);}
           
    else if(icomp == SPH_to_SS){sprintf(comp_name, hd_ss);}
    else if(icomp == SPH_to_SP){sprintf(comp_name, hd_sp);}
    else if(icomp == SPH_to_SZ){sprintf(comp_name, hd_sz);}
    else if(icomp == SPH_to_PZ){sprintf(comp_name, hd_pz);}
    else if(icomp == SPH_to_ZZ){sprintf(comp_name, hd_zz);}
            
    else if(icomp == SPH_to_XX){sprintf(comp_name, hd_xx);}
    else if(icomp == SPH_to_XY){sprintf(comp_name, hd_xy);}
    else if(icomp == SPH_to_XZ){sprintf(comp_name, hd_xz);}
    else if(icomp == SPH_to_YY){sprintf(comp_name, hd_yy);}
    else if(icomp == SPH_to_YZ){sprintf(comp_name, hd_yz);}
    
    else if(icomp == SPH_to_PR){sprintf(comp_name, hd_rp);}
    else if(icomp == SPH_to_ZS){sprintf(comp_name, hd_zs);}
    else if(icomp == SPH_to_ZX){sprintf(comp_name, hd_zx);}
    return;
};


int find_comp_id_from_by_cyl_tensor(const char *selected){
    if     (cmp_no_case_c(selected, hd_ss) != 0){return CYL_to_SS;}
    else if(cmp_no_case_c(selected, hd_sp) != 0){return CYL_to_SP;}
    else if(cmp_no_case_c(selected, hd_sz) != 0){return CYL_to_SZ;}
    else if(cmp_no_case_c(selected, hd_pp) != 0){return CYL_to_PP;}
    else if(cmp_no_case_c(selected, hd_pz) != 0){return CYL_to_PZ;}
    else if(cmp_no_case_c(selected, hd_zz) != 0){return CYL_to_ZZ;}
    
    else if(cmp_no_case_c(selected, hd_rr) != 0){return CYL_to_RR;}
    else if(cmp_no_case_c(selected, hd_rt) != 0){return CYL_to_RT;}
    else if(cmp_no_case_c(selected, hd_rp) != 0){return CYL_to_RP;}
    else if(cmp_no_case_c(selected, hd_tt) != 0){return CYL_to_TT;}
    else if(cmp_no_case_c(selected, hd_tp) != 0){return CYL_to_TP;}
    
    else if(cmp_no_case_c(selected, hd_xx) != 0){return CYL_to_XX;}
    else if(cmp_no_case_c(selected, hd_xy) != 0){return CYL_to_XY;}
    else if(cmp_no_case_c(selected, hd_xz) != 0){return CYL_to_XZ;}
    else if(cmp_no_case_c(selected, hd_yy) != 0){return CYL_to_YY;}
    else if(cmp_no_case_c(selected, hd_yz) != 0){return CYL_to_YZ;}
    
    else if(cmp_no_case_c(selected, hd_zs) != 0){return CYL_to_ZS;}
    else if(cmp_no_case_c(selected, hd_rp) != 0){return CYL_to_PR;}
    else if(cmp_no_case_c(selected, hd_zx) != 0){return CYL_to_ZX;}
    return -1;
}

void set_component_name_by_cyl_tensor(int icomp, char *comp_name){
    if     (icomp == CYL_to_SS){sprintf(comp_name, hd_ss);}
    else if(icomp == CYL_to_SP){sprintf(comp_name, hd_sp);}
    else if(icomp == CYL_to_SZ){sprintf(comp_name, hd_sz);}
    else if(icomp == CYL_to_PP){sprintf(comp_name, hd_pp);}
    else if(icomp == CYL_to_PZ){sprintf(comp_name, hd_pz);}
    else if(icomp == CYL_to_ZZ){sprintf(comp_name, hd_zz);}
            
    else if(icomp == CYL_to_RR){sprintf(comp_name, hd_rr);}
    else if(icomp == CYL_to_RT){sprintf(comp_name, hd_rt);}
    else if(icomp == CYL_to_RP){sprintf(comp_name, hd_rp);}
    else if(icomp == CYL_to_TT){sprintf(comp_name, hd_tt);}
    else if(icomp == CYL_to_TP){sprintf(comp_name, hd_tp);}
            
    else if(icomp == CYL_to_XX){sprintf(comp_name, hd_xx);}
    else if(icomp == CYL_to_XY){sprintf(comp_name, hd_xy);}
    else if(icomp == CYL_to_XZ){sprintf(comp_name, hd_xz);}
    else if(icomp == CYL_to_YY){sprintf(comp_name, hd_yy);}
    else if(icomp == CYL_to_YZ){sprintf(comp_name, hd_yz);}
            
    else if(icomp == CYL_to_ZS){sprintf(comp_name, hd_zs);}
    else if(icomp == CYL_to_PR){sprintf(comp_name, hd_rp);}
    else if(icomp == CYL_to_ZX){sprintf(comp_name, hd_zx);}
    return;
};


int find_comp_id_from_by_xyz_tensor(const char *selected){
    if     (cmp_no_case_c(selected, hd_xx) != 0){return XYZ_to_XX;}
    else if(cmp_no_case_c(selected, hd_xy) != 0){return XYZ_to_XY;}
    else if(cmp_no_case_c(selected, hd_xz) != 0){return XYZ_to_XZ;}
    else if(cmp_no_case_c(selected, hd_yy) != 0){return XYZ_to_YY;}
    else if(cmp_no_case_c(selected, hd_yz) != 0){return XYZ_to_YZ;}
    else if(cmp_no_case_c(selected, hd_zz) != 0){return XYZ_to_ZZ;}
    
    else if(cmp_no_case_c(selected, hd_rr) != 0){return XYZ_to_RR;}
    else if(cmp_no_case_c(selected, hd_rt) != 0){return XYZ_to_RT;}
    else if(cmp_no_case_c(selected, hd_rp) != 0){return XYZ_to_RP;}
    else if(cmp_no_case_c(selected, hd_tt) != 0){return XYZ_to_TT;}
    else if(cmp_no_case_c(selected, hd_tp) != 0){return XYZ_to_TP;}
    else if(cmp_no_case_c(selected, hd_pp) != 0){return XYZ_to_PP;}
    
    else if(cmp_no_case_c(selected, hd_ss) != 0){return XYZ_to_SS;}
    else if(cmp_no_case_c(selected, hd_sp) != 0){return XYZ_to_SP;}
    else if(cmp_no_case_c(selected, hd_sz) != 0){return XYZ_to_SZ;}
    else if(cmp_no_case_c(selected, hd_pz) != 0){return XYZ_to_PZ;}
    
    else if(cmp_no_case_c(selected, hd_zx) != 0){return XYZ_to_ZX;}
    else if(cmp_no_case_c(selected, hd_rp) != 0){return XYZ_to_PR;}
    else if(cmp_no_case_c(selected, hd_zs) != 0){return XYZ_to_ZS;}
    return -1;
}

void set_component_name_by_xyz_tensor(int icomp, char *comp_name){
    if      (icomp == XYZ_to_XX){sprintf(comp_name, hd_xx);}
    else if(icomp == XYZ_to_XY){sprintf(comp_name, hd_xy);}
    else if(icomp == XYZ_to_XZ){sprintf(comp_name, hd_xz);}
    else if(icomp == XYZ_to_YY){sprintf(comp_name, hd_yy);}
    else if(icomp == XYZ_to_YZ){sprintf(comp_name, hd_yz);}
    else if(icomp == XYZ_to_ZZ){sprintf(comp_name, hd_zz);}
            
    else if(icomp == XYZ_to_RR){sprintf(comp_name, hd_rr);}
    else if(icomp == XYZ_to_RT){sprintf(comp_name, hd_rt);}
    else if(icomp == XYZ_to_RP){sprintf(comp_name, hd_rp);}
    else if(icomp == XYZ_to_TT){sprintf(comp_name, hd_tt);}
    else if(icomp == XYZ_to_TP){sprintf(comp_name, hd_tp);}
    else if(icomp == XYZ_to_PP){sprintf(comp_name, hd_pp);}
            
    else if(icomp == XYZ_to_SS){sprintf(comp_name, hd_ss);}
    else if(icomp == XYZ_to_SP){sprintf(comp_name, hd_sp);}
    else if(icomp == XYZ_to_SZ){sprintf(comp_name, hd_sz);}
    else if(icomp == XYZ_to_PZ){sprintf(comp_name, hd_pz);}
            
    else if(icomp == XYZ_to_ZX){sprintf(comp_name, hd_zx);}
    else if(icomp == XYZ_to_PR){sprintf(comp_name, hd_rp);}
    else if(icomp == XYZ_to_ZS){sprintf(comp_name, hd_zs);}
    return;
};


int find_tensor_component_id(const int id_coord, const char *selected){
    if(id_coord == SPH_COORDINATE){
        return find_comp_id_from_by_sph_tensor(selected);
    }else if(id_coord == CYL_COORDINATE){
        return find_comp_id_from_by_cyl_tensor(selected);
    } else {
        return find_comp_id_from_by_xyz_tensor(selected);
    };
    return -1;
}

void set_tensor_component_name(const int id_coord, const int icomp, 
                               char *comp_name){
    if(id_coord == SPH_COORDINATE){
        set_component_name_by_sph_tensor(icomp, comp_name);
    }else if(id_coord == CYL_COORDINATE){
        set_component_name_by_cyl_tensor(icomp, comp_name);
    } else {
        set_component_name_by_xyz_tensor(icomp, comp_name);
    };
    return;
};
